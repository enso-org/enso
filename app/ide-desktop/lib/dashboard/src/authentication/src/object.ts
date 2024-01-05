/** @file Immutably shallowly merge an object with a partial update. */

// =============
// === merge ===
// =============

/** Immutably shallowly merge an object with a partial update.
 * Does not preserve classes. Useful for preserving order of properties. */
export function merge<T extends object>(object: T, update: Partial<T>): T {
    return Object.assign({ ...object }, update)
}
