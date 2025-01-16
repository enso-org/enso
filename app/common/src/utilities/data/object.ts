/** @file Functions related to manipulating objects. */

// ===============
// === Mutable ===
// ===============

/** Remove the `readonly` modifier from all fields in a type. */
export type Mutable<T> = {
  -readonly [K in keyof T]: T[K]
}

// =============
// === merge ===
// =============

/** Prevents generic parameter inference by hiding the type parameter behind a conditional type. */
type NoInfer<T> = [T][T extends T ? 0 : never]

/**
 * Immutably shallowly merge an object with a partial update.
 * Does not preserve classes. Useful for preserving order of properties.
 */
export function merge<T extends object>(object: T, update: Partial<T>): T {
  for (const [key, value] of Object.entries(update)) {
    if (!Object.is(value, (object as Record<string, unknown>)[key])) {
      // This is FINE, as the matching `return` is below this `return`.
      return Object.assign({ ...object }, update)
    }
  }
  return object
}

/** Return a function to update an object with the given partial update. */
export function merger<T extends object>(update: Partial<NoInfer<T>>): (object: T) => T {
  return (object) => merge(object, update)
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

/**
 * Return the entries of an object. UNSAFE only when it is possible for an object to have
 * extra keys.
 */
export function unsafeKeys<T extends object>(object: T): readonly (keyof T)[] {
  // @ts-expect-error This is intentionally a wrapper function with a different type.
  return Object.keys(object)
}

/** Return the values of an object. UNSAFE only when it is possible for an object to have extra keys. */
export function unsafeValues<const T extends object>(object: T): readonly T[keyof T][] {
  return Object.values(object)
}

/**
 * Return the entries of an object. UNSAFE only when it is possible for an object to have
 * extra keys.
 */
export function unsafeEntries<T extends object>(
  object: T,
): readonly { [K in keyof T]: readonly [K, T[K]] }[keyof T][] {
  // @ts-expect-error This is intentionally a wrapper function with a different type.
  return Object.entries(object)
}

/**
 * Return an object from its entries. UNSAFE only when it is possible for an object to have
 * extra keys.
 */
export function unsafeFromEntries<T extends object>(
  entries: readonly { [K in keyof T]: readonly [K, T[K]] }[keyof T][],
): T {
  // @ts-expect-error This is intentionally a wrapper function with a different type.
  return Object.fromEntries(entries)
}

// =============================
// === unsafeRemoveUndefined ===
// =============================

/** A the object with `undefined` unsafely removed from the value types of all of its keys. */
export function unsafeRemoveUndefined<T extends object>(
  object: T,
): { [K in keyof T]: Exclude<T[K], undefined> } {
  // This function intentionally performs an mostly safe, but ultimately unsafe cast.
  return object as never
}

// ==================
// === mapEntries ===
// ==================

/**
 * Return the entries of an object. UNSAFE only when it is possible for an object to have
 * extra keys.
 */
export function mapEntries<K extends PropertyKey, V, W>(
  object: Record<K, V>,
  map: (key: K, value: V) => W,
): Readonly<Record<K, W>> {
  // @ts-expect-error It is known that the set of keys is the same for the input and the output,
  // because the output is dynamically generated based on the input.
  return Object.fromEntries(
    unsafeEntries(object).map<[K, W]>((kv) => {
      const [k, v] = kv
      return [k, map(k, v)]
    }),
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

// ============
// === omit ===
// ============

/** UNSAFE when `Ks` contains strings that are not in the runtime array. */
export function omit<T, Ks extends readonly [string & keyof T, ...(string & keyof T)[]]>(
  object: T,
  ...keys: Ks
): Omit<T, Ks[number]> {
  const keysSet = new Set<string>(keys)
  return Object.fromEntries(
    // This is SAFE, as it is a readonly upcast.
    Object.entries(object as Readonly<Record<string, unknown>>).filter(([k]) => !keysSet.has(k)),
  ) as Omit<T, Ks[number]>
}

// ============
// === pick ===
// ============

/** UNSAFE when `Ks` contains strings that are not in the runtime array. */
export function pick<T, Ks extends readonly [string & keyof T, ...(string & keyof T)[]]>(
  object: T,
  ...keys: Ks
): Pick<T, Ks[number]> {
  const keysSet = new Set<string>(keys)
  return Object.fromEntries(
    // This is SAFE, as it is a readonly upcast.
    Object.entries(object as Readonly<Record<string, unknown>>).filter(([k]) => keysSet.has(k)),
  ) as Pick<T, Ks[number]>
}

// ===================
// === ExtractKeys ===
// ===================

/** Filter a type `T` to include only the properties extending the given type `U`. */
export type ExtractKeys<T, U> = {
  [K in keyof T]: T[K] extends U ? K : never
}[keyof T]

// ================
// === MethodOf ===
// ================

/** An instance method of the given type. */
export type MethodOf<T> = (this: T, ...args: never) => unknown

// ===================
// === useObjectId ===
// ===================

/** Composable providing support for managing object identities. */
export function useObjectId() {
  let lastId = 0
  const idNumbers = new WeakMap<object, number>()
  /** @returns A value that can be used to compare object identity. */
  function objectId(o: object): number {
    const id = idNumbers.get(o)
    if (id == null) {
      lastId += 1
      idNumbers.set(o, lastId)
      return lastId
    }
    return id
  }
  return { objectId }
}

/**
 * Returns the union of `A` and `B`, with a type-level assertion that `A` and `B` don't have any keys in common; this
 * can be used to splice together objects without the risk of collisions.
 */
export type DisjointKeysUnion<A, B> = keyof A & keyof B extends never ? A & B : never

/**
 * Merge types of values of an object union. Useful to return an object that UNSAFELY
 * (at runtime) conforms to the shape of a discriminated union.
 * Especially useful for things like Tanstack Query results.
 */
export type MergeValuesOfObjectUnion<T> = {
  [K in `${keyof T & string}`]: T[K & keyof T]
}
