/** An array that has at least one element present at all times. */
export type NonEmptyArray<T> = [T, ...T[]]

/** An equivalent of `Array.prototype.findIndex` method, but returns null instead of -1. */
export function findIndexOpt<T>(arr: T[], pred: (elem: T) => boolean): number | null {
  const index = arr.findIndex(pred)
  return index >= 0 ? index : null
}

/** Returns the index of the partition point according to the given predicate
 * (the index of the first element of the second partition).
 *
 * The array is assumed to be partitioned according to the given predicate.
 * This means that all elements for which the predicate returns `true` are at the start of the array
 * and all elements for which the predicate returns `false` are at the end.
 * For example, `[7, 15, 3, 5, 4, 12, 6]` is partitioned under the predicate `x % 2 != 0`
 * (all odd numbers are at the start, all even at the end).
 *
 * If this array is not partitioned, the returned result is unspecified and meaningless,
 * as this method performs a kind of binary search.
 *
 * @see The original docs for the equivalent function in Rust: {@link https://doc.rust-lang.org/std/primitive.slice.html#method.partition_point} */
export function partitionPoint<T>(
  array: T[],
  pred: (elem: T) => boolean,
  start = 0,
  end = array.length,
): number {
  while (start < end) {
    // Shift right by one to halve and round down in the same step.
    const middle = (start + end) >> 1
    if (pred(array[middle]!)) start = middle + 1
    else end = middle
  }
  return start
}

/** Index into an array using specified index. When the index is nullable, returns undefined. */
export function tryGetIndex<T>(arr: T[], index: number | undefined | null): T | undefined {
  return index == null ? undefined : arr[index]
}
