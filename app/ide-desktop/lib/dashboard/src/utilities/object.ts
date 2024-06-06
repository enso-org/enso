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

// =======================
// === singleKeyObject ===
// =======================

/** Evaluates to `T` if it is not a union, else `never`. */
type MustNotBeUnion<T> = true extends (T extends T ? true : never) ? never : T

// This symbol is used in `MustBeLiteral` - however it is never needed at runtime.
// eslint-disable-next-line no-restricted-syntax
declare const EXAMPLE_SYMBOL: unique symbol

/** Evaluates to `T` if it is a literal type, else `never`.
 * Compares with two values of each type. Individual values cannot possibly match both values,
 * so they will pass this test. Tests like `string extends T` normally work, but they fail for
 * branded types like `string & { _foo: "bar"; }`, so they cannot be used here where branded types
 * are used liberally. */
type MustBeLiteral<T> = T extends string
  ? T & '' extends never
    ? T
    : T & '_' extends never
      ? T
      : never
  : T extends number
    ? T & 0 extends never
      ? T
      : T & 1 extends never
        ? T
        : never
    : T extends boolean
      ? T & true extends never
        ? T
        : T & false extends never
          ? T
          : never
      : T extends symbol
        ? T & typeof EXAMPLE_SYMBOL extends never
          ? T
          : never
        : never

/** Evaluates to `T` if it is a literal type, else `never`.
 * Compares with two values of each type. Individual values cannot possibly match both values,
 * so they will pass this test. Tests like `string extends T` normally work, but they fail for
 * branded types like `string & { _foo: "bar"; }`, so they cannot be used here where branded types
 * are used liberally. */
type MustNotBeLiteral<T> = T extends string
  ? T & '' extends never
    ? never
    : T & '_' extends never
      ? never
      : T
  : T extends number
    ? T & 0 extends never
      ? never
      : T & 1 extends never
        ? never
        : T
    : T extends boolean
      ? T & true extends never
        ? never
        : T & false extends never
          ? never
          : T
      : T extends symbol
        ? T & typeof EXAMPLE_SYMBOL extends never
          ? never
          : T
        : T

export function singleKeyObject<K extends PropertyKey, V>(
  key: MustBeLiteral<K> & MustNotBeUnion<K>,
  value: V
): Record<K, V>
export function singleKeyObject<K extends PropertyKey, V>(
  // The signatures SHOULD NOT be merged - while it is probably not possible for a value to be
  // both `MustNotBeLiteral` and `MustBeLiteral & MustNotBeUnion`, it is safer to assume that
  // it may happen.
  // eslint-disable-next-line @typescript-eslint/unified-signatures
  key: MustNotBeLiteral<K>,
  value: V
): Record<K, V>
export function singleKeyObject<K extends PropertyKey, V>(key: K, value: V): Partial<Record<K, V>>
/** Return an object with a single key. */
export function singleKeyObject<K extends PropertyKey, V>(key: K, value: V) {
  // This is SAFE - but TypeScript widens the type of `key` to `string` when it is used as a
  // computed key.
  // eslint-disable-next-line no-restricted-syntax
  return { [key]: value } as Partial<Record<K, V>>
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

// ===================
// === fromEntries ===
// ===================

/** Construct a {@link Record} from an array of key-value pairs. */
export function fromEntries<K extends PropertyKey, V>(
  iterable: Iterable<readonly [MustNotBeLiteral<K>, V]>
) {
  // This is SAFE as `K` is known to not be a literal type.
  // eslint-disable-next-line no-restricted-syntax
  return Object.fromEntries(iterable) as Record<K, V>
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

// ============
// === omit ===
// ============

/** UNSAFE when `Ks` contains strings that are not in the runtime array. */
export function omit<T, Ks extends readonly (string & keyof T)[] | []>(
  object: T,
  ...keys: Ks
): Omit<T, Ks[number]> {
  const keysSet = new Set<string>(keys)
  // eslint-disable-next-line no-restricted-syntax
  return Object.fromEntries(
    // This is SAFE, as it is a reaonly upcast.
    // eslint-disable-next-line no-restricted-syntax
    Object.entries(object as Readonly<Record<string, unknown>>).flatMap(kv =>
      !keysSet.has(kv[0]) ? [kv] : []
    )
  ) as Omit<T, Ks[number]>
}
