/** An array that has at least one element present at all times. */
export type NonEmptyArray<T> = [T, ...T[]]

/** An equivalent of `Array.prototype.findIndex` method, but returns null instead of -1. */
export function findIndexOpt<T>(arr: T[], pred: (elem: T) => boolean): number | null {
  const index = arr.findIndex(pred)
  return index >= 0 ? index : null
}
