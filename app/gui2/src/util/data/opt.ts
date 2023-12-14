/** @file A value that may be `null` or `undefined`. */

/** Optional value type. This is a replacement for `T | null | undefined` that is more
 * convenient to use. We do not select a single value to represent "no value", because we are using
 * libraries that disagree whether `null` (e.g. Yjs) or `undefined` (e.g. Vue) should be used for
 * that purpose. We want to be compatible with both without needless conversions. In our own code,
 * we should return `undefined` for "no value", since that is the default value for empty or no
 * `return` expression. In order to test whether an `Opt<T>` is defined or not, use `x == null` or
 * `isSome` function.
 *
 * Note: For JSON-serialized data, prefer explicit `null` over `undefined`, since `undefined` is
 * not serializable. Alternatively, use optional field syntax (e.g. `{ x?: number }`). */
export type Opt<T> = T | null | undefined

export function isSome<T>(value: Opt<T>): value is T {
  return value != null
}

export function isNone(value: Opt<any>): value is null | undefined {
  return value == null
}
