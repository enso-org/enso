/** An array that has at least one element present at all times. */
export type NonEmptyArray<T> = [T, ...T[]]

/** An equivalent of `Array.prototype.findIndex` method, but returns null instead of -1. */
export function findIndexOpt<T>(arr: T[], pred: (elem: T) => boolean): number | null {
  const index = arr.findIndex(pred)
  return index >= 0 ? index : null
}

/** Binary search, using a predicate `pred` that returns `false` for elements ordered before the
 * target, and `true` for elements ordered after the target. Returns the index of the first element
 * for which the predicate returns `true`.
 *
 * The array MUST be sorted with respect to the predicate. */
export function binarySearch<T>(
  array: T[],
  pred: (elem: T) => boolean,
  start = 0,
  end = array.length,
): number {
  while (start < end) {
    // Shift right by one to halve and round down in the same step.
    const middle = (start + end) >> 1
    if (pred(array[middle]!)) end = middle
    else start = middle + 1
  }
  return start
}
