/** @file Functions related to writing values to objects. */

/**
 * "Unsafe" because it bypasses React Compiler checks.
 * This function exists to bypass the React Compiler expecting values
 * (`document`, `window`, object refs passed in) to not be mutated.
 */
export function unsafeWriteValue<T extends object, K extends keyof T>(
  object: T,
  key: K,
  value: T[K],
) {
  object[key] = value
}
