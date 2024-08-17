// L3-eval.ts
// Evaluator with Environments model

import { map } from "ramda";
import { isBoolExp, isCExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef,
         isAppExp, isDefineExp, isIfExp, isLetExp, isProcExp, isClassExp,
         Binding, VarDecl, CExp, Exp, IfExp, LetExp, ProcExp, ClassExp, Program,
         parseL3Exp,  DefineExp,
         unparseL3} from "./L3-ast";
import { applyEnv, makeEmptyEnv, makeExtEnv, Env } from "./L3-env-env";
import { isClosure, makeClosureEnv, Closure, Value, Class, Object, makeClass, makeObject, isClass, isObject, valueToString, SExpValue } from "./L3-value";
import { applyPrimitive } from "./evalPrimitive";
import { allT, first, rest, isEmpty, isNonEmptyList } from "../shared/list";
import { Result, makeOk, makeFailure, bind, mapResult } from "../shared/result";
import { parse as p } from "../shared/parser";
import { format } from "../shared/format";

// ========================================================
// Eval functions

const applicativeEval = (exp: CExp, env: Env): Result<Value> =>{
    return isNumExp(exp) ? makeOk(exp.val) :
    isBoolExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? makeOk(exp.val) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isClassExp(exp) ? evalClass(exp, env) :
    isLetExp(exp) ? evalLet(exp, env) :
    isAppExp(exp) ? bind(applicativeEval(exp.rator, env),
                      (proc: Value) =>
                        bind(mapResult((rand: CExp) => 
                           applicativeEval(rand, env), exp.rands),
                              (args: Value[]) =>
                                 applyProcedure(proc, args))) :
    makeFailure('"let" not supported (yet)');
}

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

/*

(class (a b) 
    ((first (lambda () a)) 
     (second (lambda () b)) 
     (sum (lambda () (+ a b)))))

()
(let 
((y (* x 3)) (x 5)) 
())

(if (> 4 3)
    (#t)
    (#f)))

(define pair
    (class (a b)
        ((first (lambda () a))
        (second (lambda () b))
        (sum (lambda () (+ a b)))
        (scale (lambda (k) (pair (* k a) (* k b))))
    ))
)

(define p34 (pair 3 4))
Eval "pair" ==> Class
Eval "p34" ==> Eval "(pair 3 4)" ==> Object
(p34 'first)
*/
const evalIf = (exp: IfExp, env: Env): Result<Value> =>
    bind(applicativeEval(exp.test, env), (test: Value) => 
            isTrueValue(test) ? applicativeEval(exp.then, env) : 
            applicativeEval(exp.alt, env));

const unparseLExps = (les: Exp[]): string =>
    map(unparseL3, les).join(" ");
                     
const evalProc = (exp: ProcExp, env: Env): Result<Closure> =>{
    return makeOk(makeClosureEnv(exp.args, exp.body, env));
}

const evalClass = (exp: ClassExp, env: Env): Result<Class> =>
    bind(
        mapResult((binding: Binding) =>
            bind(applicativeEval(binding.val, env), (val: Value) =>
                makeOk({ tag: "Binding", var: binding.var, val } as Binding)
            ), exp.methods
        ), 
        (methods: Binding[]) => makeOk(makeClass(exp.fields, methods, env))
    );
    
// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value, args: Value[]): Result<Value> =>{

    return isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args) :
    isClass(proc) ? applyClass(proc, args) :
    isObject(proc) ? applyObject(proc, args) :
    makeFailure(`Bad procedure ${format(proc)}`);
}

const applyClass = (c: Class, values: Value[]): Result<Object> =>{
    return values.length === c.fields.length ? makeOk(makeObject(c, values)) : makeFailure(`Failed to create class: expected number of values: ${c.fields.length}, given ${values.length}`);
}
const applyObject = (o: Object, values: Value[]): Result<Value> => {
    if(values.length === 0)
        return makeFailure(`Failed to call object's method: no method given`);
    const methodName = valueToString(values[0]);
    const method = o.class.methods.find(m => m.var.var === methodName);
    if (!method)
        return makeFailure(`Unrecognized method: ${methodName}`);
    
    const methodExp = method.val;

    if (isClosure(methodExp)) {
        const methodProc = methodExp as Closure;
        const closureVars = map((v: VarDecl) => v.var, methodProc.params);
        const closureArgValues = values.slice(1);

        const classVars = o.class.fields.map(f => f.var);
        const objectVals  = o.values;
        
        const allVars = [...closureVars, ...classVars]; //[a, b, k]
        const allVals = [...closureArgValues, ...objectVals]; //[3, 4, 2]
        
        const newEnv = makeExtEnv(allVars, allVals, o.class.env);
        return evalSequence(methodProc.body, newEnv);
    }

    return makeFailure(`Failed to call object's method: method is not valid`);
}

const applyClosure = (proc: Closure, args: Value[]): Result<Value> => {
    //
    const vars = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env));
}

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: Exp[], env: Env): Result<Value> =>
    isNonEmptyList<Exp>(seq) ? evalCExps(first(seq), rest(seq), env) : 
    makeFailure("Empty sequence");
    
const evalCExps = (first: Exp, rest: Exp[], env: Env): Result<Value> =>
    isDefineExp(first) ? evalDefineExps(first, rest, env) :
    isCExp(first) && isEmpty(rest) ? applicativeEval(first, env) :
    isCExp(first) ? bind(applicativeEval(first, env), _ => evalSequence(rest, env)) :
    first;
    
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: DefineExp, exps: Exp[], env: Env): Result<Value> =>
    bind(applicativeEval(def.val, env), (rhs: Value) => 
            evalSequence(exps, makeExtEnv([def.var.var], [rhs], env)));


// Main program
export const evalL3program = (program: Program): Result<Value> =>
    evalSequence(program.exps, makeEmptyEnv());

export const evalParse = (s: string): Result<Value> =>
    bind(p(s), (x) => 
        bind(parseL3Exp(x), (exp: Exp) =>
            evalSequence([exp], makeEmptyEnv())));

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env): Result<Value> => {
    const vals  = mapResult((v: CExp) => 
        applicativeEval(v, env), map((b: Binding) => b.val, exp.bindings));
    const vars = map((b: Binding) => b.var.var, exp.bindings);
    return bind(vals, (vals: Value[]) => 
        evalSequence(exp.body, makeExtEnv(vars, vals, env)));
}
