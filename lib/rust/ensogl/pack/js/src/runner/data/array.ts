export function arrayIntoTuples<T>(arr: T[]): [T, T][] {
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

export function zip<T, S>(arr1: T[], arr2: S[]): [T, S][] {
    // @ts-ignore
    return [...arr1].map((_, c) => [arr1, arr2].map(row => row[c]))
}
