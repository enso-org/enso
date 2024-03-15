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

// ================
// === readonly ===
// ================

/** Makes all properties readonly at the type level. They are still mutable at the runtime level. */
export function readonly<T extends object>(object: T): Readonly<T> {
  return object
}

// =====================
// === unsafeMutable ===
// =====================

/** Removes the readonly modifier from all properties on the object. UNSAFE. */
export function unsafeMutable<T extends object>(object: T): { -readonly [K in keyof T]: T[K] } {
  return object
}

// =====================
// === unsafeEntries ===
// =====================

/** Return the entries of an object. UNSAFE only when it is possible for an object to have
 * extra keys. */
export function unsafeEntries<T extends object>(
  object: T
): readonly { [K in keyof T]: readonly [K, T[K]] }[keyof T][] {
  // @ts-expect-error This is intentionally a wrapper function with a different type.
  return Object.entries(object)
}

// ==================
// === mapEntries ===
// ==================

/** Return the entries of an object. UNSAFE only when it is possible for an object to have
 * extra keys. */
export function mapEntries<K extends PropertyKey, V, W>(
  object: Record<K, V>,
  map: (key: K, value: V) => W
): Readonly<Record<K, W>> {
  // @ts-expect-error It is known that the set of keys is the same for the input and the output,
  // because the output is dynamically generated based on the input.
  return Object.fromEntries(
    unsafeEntries(object).map<[K, W]>(kv => {
      const [k, v] = kv
      return [k, map(k, v)]
    })
  )
}

// ================
// === asObject ===
// ================

/** Either return the object unchanged, if the input was an object, or `null`. */
export function asObject(value: unknown): object | null {
  return typeof value === 'object' && value != null ? value : null
}

// =============================
// === singletonObjectOrNull ===
// =============================

/** Either return a singleton object, if the input was an object, or an empty array. */
export function singletonObjectOrNull(value: unknown): [] | [object] {
  return typeof value === 'object' && value != null ? [value] : []
}
