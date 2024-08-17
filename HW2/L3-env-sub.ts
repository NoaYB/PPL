// ========================================================
// Environment data type for L3
import { Value } from './L3-value';
import { Result, makeFailure, makeOk } from '../shared/result';

export type Env = EmptyEnv | NonEmptyEnv;
export type EmptyEnv = {tag: "EmptyEnv", id: 0 }
export type NonEmptyEnv = {
    tag: "Env";
    var: string;
    val: Value;
    nextEnv: Env;
    id: 0;
}
export const makeEmptyEnv = (): EmptyEnv => ({tag: "EmptyEnv", id: 0});
export const makeEnv = (v: string, val: Value, env: Env): NonEmptyEnv =>
    ({tag: "Env", var: v, val: val, nextEnv: env, id: 0});
export const isEmptyEnv = (x: any): x is EmptyEnv => x.tag === "EmptyEnv";
export const isNonEmptyEnv = (x: any): x is NonEmptyEnv => x.tag === "Env";
export const isEnv = (x: any): x is Env => isEmptyEnv(x) || isNonEmptyEnv(x);

/*
PS 5:
An environment represents a partial function (as opposed to a total function) from symbols (variable names) 
to values. It supports the operation apply-env(env, var) which either returns the value of var in the 
environment env, or else throws an error.
*/
export const applyEnv = (env: Env, v: string): Result<Value> =>
    isEmptyEnv(env) ? makeFailure(`var not found: ${v}`) :
    env.var === v ? makeOk(env.val) :
    applyEnv(env.nextEnv, v);

