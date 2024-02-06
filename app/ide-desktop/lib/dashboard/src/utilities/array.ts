/** @file Utilities for manipulating arrays. */

// ====================
// === shallowEqual ===
// ====================

/** Whether both arrays contain the same items. Does not recurse into the items. */
export function shallowEqual<T>(a: readonly T[], b: readonly T[]) {
  return a.length === b.length && a.every((item, i) => item === b[i])
}

// ================
// === includes ===
// ================

/** Returns a type predicate that returns true if and only if the value is in the array.
 * The array MUST contain every element of `T`. */
export function includes<T>(array: T[], item: unknown): item is T {
  const arrayOfUnknown: unknown[] = array
  return arrayOfUnknown.includes(item)
}

/** Returns a type predicate that returns true if and only if the value is in the array.
 * The array MUST contain every element of `T`. */
export function includesPredicate<T>(array: Iterable<T>) {
  const set: Set<unknown> = array instanceof Set ? array : new Set<T>(array)
  return (item: unknown): item is T => set.has(item)
}

// ======================
// === splice helpers ===
// ======================

/** The value returned when {@link Array.findIndex} fails. */
const NOT_FOUND = -1

/** Insert items before the first index `i` for which `predicate(array[i])` is `true`.
 * Insert the items at the end if the `predicate` never returns `true`. */
export function spliceBefore<T>(array: T[], items: T[], predicate: (value: T) => boolean) {
  const index = array.findIndex(predicate)
  array.splice(index === NOT_FOUND ? array.length : index, 0, ...items)
  return array
}

/** Return a copy of the array, with items inserted before the first index `i` for which
 * `predicate(array[i])` is `true`. The items are inserted at the end if the `predicate` never
 * returns `true`. */
export function splicedBefore<T>(array: T[], items: T[], predicate: (value: T) => boolean) {
  return spliceBefore(Array.from(array), items, predicate)
}

/** Insert items after the first index `i` for which `predicate(array[i])` is `true`.
 * Insert the items at the end if the `predicate` never returns `true`. */
export function spliceAfter<T>(array: T[], items: T[], predicate: (value: T) => boolean) {
  const index = array.findIndex(predicate)
  array.splice(index === NOT_FOUND ? array.length : index + 1, 0, ...items)
  return array
}

/** Return a copy of the array, with items inserted after the first index `i` for which
 * `predicate(array[i])` is `true`. The items are inserted at the end if the `predicate` never
 * returns `true`. */
export function splicedAfter<T>(array: T[], items: T[], predicate: (value: T) => boolean) {
  return spliceAfter(Array.from(array), items, predicate)
}
