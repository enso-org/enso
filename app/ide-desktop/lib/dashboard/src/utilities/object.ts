/** @file Immutably shallowly merge an object with a partial update. */

// =============
// === merge ===
// =============

/** Prevents generic parameter inference by hiding the type parameter behind a conditional type. */
type NoInfer<T> = [T][T extends T ? 0 : never]

/** Immutably shallowly merge an object with a partial update.
 * Does not preserve classes. Useful for preserving order of properties. */
export function merge<T extends object>(object: T, update: Partial<T>): T {
    return Object.assign({ ...object }, update)
}

/** Return a function to update an object with the given partial update. */
export function merger<T extends object>(update: Partial<NoInfer<T>>): (object: T) => T {
    return object => Object.assign({ ...object }, update)
}

// =====================
// === unsafeMutable ===
// =====================

/** Removes the `readonly` modifier from all of an object's properties. */
type Mutable<T> = { -readonly [K in keyof T]: T[K] }

/** Removes `readonly` modifiers from an object. */
export function unsafeMutable<T extends object>(object: T): Mutable<T> {
    return object
}
