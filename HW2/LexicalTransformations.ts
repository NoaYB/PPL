import { ClassExp, ProcExp, Exp, Program, VarDecl, makeProcExp, makeVarDecl, IfExp, Binding, makeIfExp, makeBoolExp, makePrimOp, makeLitExp, makeVarRef, makeAppExp, isExp, isProgram, makeProgram, isProcExp, isCExp, isAtomicExp, isLitExp, isIfExp, isAppExp, isClassExp, CExp, isDefineExp, makeDefineExp } from "./L3-ast";
import { Result, makeFailure, makeOk } from "../shared/result";
import { map } from "ramda";
import { makeClosure, makeSymbolSExp } from "./L3-value";

/*
Purpose: Transform ClassExp to ProcExp
Signature: class2proc(classExp)
Type: ClassExp => ProcExp
*/

/*
(define pair 
   (class (a b) 
      ((first (lambda () a)) 
       (second (lambda () b))
       (sum (lambda () (+ a b)))
      )
    )
)
(define p34 (pair 3 4))
(p34 'sum)

(define pair
    (lambda (a b)
        (lambda (msg)
            (if (eq? msg 'first)
                ((lambda () a) )
                (if (eq? msg 'second)
                    ((lambda () b) )
                    (if (eq? msg 'sum)
                        ((lambda () (+ a b)) )
                        #f))))))


*/
/*
export type IfExp = {tag: "IfExp"; test: CExp; then: CExp; alt: CExp; }
export type ProcExp = {tag: "ProcExp"; args: VarDecl[], body: CExp[]; }
export type ClassExp = {tag: "ClassExp"; fields: VarDecl[], methods: Binding[]; }
export type Binding = {tag: "Binding"; var: VarDecl; val: CExp; }
*/
export const class2proc = (exp: ClassExp): ProcExp => {
    
    const args = exp.fields;
    const body = makeProcExp([makeVarDecl("msg")], [bindings2if(exp.methods)]);
    // {tag: "ProcExp", args: exp.fields, body: []}
    return makeProcExp(args, [body]);

};

const bindings2if = (bindings: Binding[]): IfExp => {
    const binding = bindings[0];
    const test = makeAppExp(makePrimOp("eq?"), [makeVarRef("msg"), makeLitExp(makeSymbolSExp(binding.var.var))]); // msg === binding.var ?
    const then = makeAppExp(binding.val, []);
    const alt = bindings.length === 1 ? makeBoolExp(false) : bindings2if(bindings.slice(1));
    return makeIfExp(test, then, alt);
};


/*
Purpose: Transform all class forms in the given AST to procs
Signature: lexTransform(AST)
Type: [Exp | Program] => Result<Exp | Program>
*/

export const lexTransform = (exp: Exp | Program): Result<Exp | Program> =>
    isExp(exp) ? makeOk(rewriteAllClassExp(exp)) :
    isProgram(exp) ? makeOk(makeProgram(map(rewriteAllClassExp, exp.exps))) :
    exp;

export const rewriteAllClass = (exp: Program | Exp): Program | Exp =>
    isExp(exp) ? rewriteAllClassExp(exp) :
    isProgram(exp) ? makeProgram(map(rewriteAllClassExp, exp.exps)) :
    exp;

const rewriteAllClassExp = (exp: Exp): Exp =>
    isCExp(exp) ? rewriteAllClassCExp(exp) :
    isDefineExp(exp) ? makeDefineExp(exp.var, rewriteAllClassCExp(exp.val)) :
    //isProcExp(exp) ? makeProcExp(exp.var, rewriteAllClassCExp(exp.val)) :
    exp;

const rewriteAllClassCExp = (exp: CExp): CExp =>
    isAtomicExp(exp) ? exp :
    isLitExp(exp) ? exp :
    isIfExp(exp) ? makeIfExp(rewriteAllClassCExp(exp.test),
                             rewriteAllClassCExp(exp.then),
                             rewriteAllClassCExp(exp.alt)) :
    isAppExp(exp) ? makeAppExp(rewriteAllClassCExp(exp.rator),
                               map(rewriteAllClassCExp, exp.rands)) :
    isProcExp(exp) ? makeProcExp(exp.args, map(rewriteAllClassCExp, exp.body)) :
    isClassExp(exp) ? rewriteAllClassCExp(class2proc(exp)) :
    exp;
    
    //all class --> proc

    //in the following example from lecture notes: 
    //all let --> lambda
/*
export const rewriteAllLet = (exp: Program | Exp): Program | Exp =>
    isExp(exp) ? rewriteAllLetExp(exp) :
    isProgram(exp) ? makeProgram(map(rewriteAllLetExp, exp.exps)) :
    exp;


const rewriteAllLetExp = (exp: Exp): Exp =>
    isCExp(exp) ? rewriteAllLetCExp(exp) :
    isDefineExp(exp) ? makeDefineExp(exp.var, rewriteAllLetCExp(exp.val)) :
    exp;


const rewriteAllLetCExp = (exp: CExp): CExp =>
    isAtomicExp(exp) ? exp :
    isLitExp(exp) ? exp :
    isIfExp(exp) ? makeIfExp(rewriteAllLetCExp(exp.test),
                             rewriteAllLetCExp(exp.then),
                             rewriteAllLetCExp(exp.alt)) :
    isAppExp(exp) ? makeAppExp(rewriteAllLetCExp(exp.rator),
                               map(rewriteAllLetCExp, exp.rands)) :
    isProcExp(exp) ? makeProcExp(exp.args, map(rewriteAllLetCExp, exp.body)) :
    isLetExp(exp) ? rewriteAllLetCExp(rewriteLet(exp)) :
    exp;
*/