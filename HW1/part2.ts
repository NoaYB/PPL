import * as R from "ramda";

const stringToArray = R.split("");


const vowels : string[] = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'];

type parenthesesAnswer = {
    count : number[];
    answer : boolean;
}

const parentheses : string[] = ['{', '}', '(', ')', '[', ']'];

/* Question 1 */
export const countVowels = (str : string) : number => stringToArray(str).filter((char: string)  => vowels.indexOf(char) !== -1).length;

/* Question 2 */
export const isPaired = (str: string): boolean => {
    const answer: parenthesesAnswer = 
        stringToArray(str)
        .filter((char: string) => parentheses.indexOf(char) !== -1)
        .reduce((answer: parenthesesAnswer, char: string) => {
            const index = parentheses.indexOf(char);
            if (index % 2 === 0) {
                answer.count[index / 2]++;
            } else {
                const openingIndex = (index - 1) / 2;
                answer.answer = answer.answer && answer.count[openingIndex] > 0;
                answer.count[openingIndex]--;
            }
            return answer;
        }, {
            count: [0, 0, 0],
            answer: true
        });

    return answer.answer && answer.count.every(count => count === 0);
};


/* Question 3 */
export type WordTree = {
    root: string;
    children: WordTree[];
}

export const treeToSentence = (tree : WordTree) : string => tree.root + tree.children.reduce((str : string, children : WordTree) => str = str + " " + treeToSentence(children), "")

