import { Result, makeFailure, makeOk, bind, either } from "../lib/result";

/* Library code */
const findOrThrow = <T>(pred: (x: T) => boolean, a: T[]): T => {
    for (let i = 0; i < a.length; i++) {
        if (pred(a[i])) return a[i];
    }
    throw "No element found.";
}

export const findResult = <T>(pred: (x: T) => boolean, a: T[]): Result<T> => {
    for (let i = 0; i < a.length; i++) {
        if (pred(a[i])) return makeOk(a[i]);
    }
    return makeFailure("No element found.");
}

/* Client code */
const returnSquaredIfFoundEven_v1 = (a: number[]): number => {
    try {
        const x = findOrThrow(x => x % 2 === 0, a);
        return x * x;
    } catch (e) {
        return -1;
    }
}

export const returnSquaredIfFoundEven_v2 = (a: number[]) : Result<number> => bind(findResult((num : number) => num%2 === 0, a), (r : number) => makeOk(r*r));

export const returnSquaredIfFoundEven_v3 = (a: number[]) : number => either(findResult((num : number) => num%2 === 0, a), (num : number) => num*num, (str : string) => -1);