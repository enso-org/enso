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

/** Either return the object unchanged, if the input was an object, or `null`. */
export function asObject(value: unknown): object | null {
  return typeof value === 'object' && value != null ? value : null
}

/** Either return a singleton object, if the input was an object, or an empty array. */
export function singletonObjectOrNull(value: unknown): [] | [object] {
  return typeof value === 'object' && value != null ? [value] : []
}
