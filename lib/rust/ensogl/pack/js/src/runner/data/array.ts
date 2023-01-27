/** @file Array extensions. */

/** Converts an array into tuples. Return null if the provided array element count is not even. */
export function arrayIntoTuples<T>(arr: T[]): [T, T][] | null {
    if (arr.length % 2 !== 0) {
        return null
    } else {
        const tuples: [T, T][] = []
        for (let i = 0; i < arr.length; i += 2) {
            const elem1 = arr[i]
            const elem2 = arr[i + 1]
            if (elem1 != null && elem2 != null) {
                tuples.push([elem1, elem2])
            }
        }
        return tuples
    }
}

/** Zips two arrays and returns an array of tuples. */
export function zip<T, S>(arr1: T[], arr2: S[]): [T, S][] {
    // @ts-expect-error
    return [...arr1].map((_, c) => [arr1, arr2].map(row => row[c]))
}
