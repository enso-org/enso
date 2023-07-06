/** @file Utility functions for manipulating {@link Set}s. */

/** Adds the value if `presence` is `true`; deletes the value if `presence` is `false`.
 *
 * Mutates and returns the given {@link Set}. */
export function setPresence<T>(set: Set<T>, value: T, presence: boolean) {
    if (presence) {
        set.add(value)
    } else {
        set.delete(value)
    }
    return set
}

/** Adds the value if `presence` is `true`; deletes the value if `presence` is `false`.
 *
 * This is an immutable version of {@link setPresence}, so it returns a new set if the old set
 * would have been mutated, and returns the original set if it would not have been mutated. */
export function withPresence<T>(set: Set<T>, value: T, presence: boolean) {
    if (presence === set.has(value)) {
        return set
    } else {
        return setPresence(new Set([...set]), value, presence)
    }
}
