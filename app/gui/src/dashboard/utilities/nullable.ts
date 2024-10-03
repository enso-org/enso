/** @file Functions related to nullable types. */

/** Call a transformation function when the value is non-nullish. */
// eslint-disable-next-line @typescript-eslint/ban-types
export function mapNonNullish<T extends {}, R>(value: T | null | undefined, map: (value: T) => R) {
  return value != null ? map(value) : value
}
