/** @file Utilities for manipulating arrays. */

/** Insert items before the first index `i` for which `predicate(array[i])` is `true`.
 * Insert the items at the end if the `predicate` never returns `true`. */
export function insertItemsAtBoundary<T>(array: T[], items: T[], predicate: (value: T) => boolean) {
    for (let i = 0; i < array.length; i += 1) {
        // This is SAFE, as `i` is guaranteed to be in bounds by the `for`-loop above.
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        if (predicate(array[i]!)) {
            array.splice(i, 0, ...items)
            // This early return is intentional, to avoid complex logic.
            // eslint-disable-next-line no-restricted-syntax
            return array
        }
    }
    array.push(...items)
    return array
}

/** Return a copy of the array, with items inserted before the first index `i` for which
 * `predicate(array[i])` is `true`. The items are inserted at the end if the `predicate` never
 * returns `true`. */
export function withItemsInsertedAtBoundary<T>(
    array: T[],
    items: T[],
    predicate: (value: T) => boolean
) {
    return insertItemsAtBoundary(Array.from(array), items, predicate)
}
