/** An array that has at least one element present at all times. */
export type NonEmptyArray<T> = [T, ...T[]]
