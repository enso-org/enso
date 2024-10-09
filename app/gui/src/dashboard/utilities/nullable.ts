/** @file Functions related to nullable types. */

/** Call a transformation function when the value is non-nullish. */
export function mapNonNullish<T extends NonNullable<unknown>, R>(
  value: T | null | undefined,
  map: (value: T) => R,
) {
  return value != null ? map(value) : value
}
