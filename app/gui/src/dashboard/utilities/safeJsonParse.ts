/**
 * @file
 *
 * A utility function to safely parse a JSON string.
 * returns the default value if the JSON string is invalid.
 * Also provides a type for the parsed JSON.
 */

import { ZodSchema } from 'zod'

/**
 * Safely parse a JSON string.
 * Parse the JSON string and return the default value if the JSON string is invalid.
 * Or if the parsed JSON does not match the type assertion.
 */
export function safeJsonParse<T = unknown>(
  value: unknown,
  defaultValue: T,
  predicate?: ZodSchema<T> | ((parsed: unknown) => parsed is T),
): T {
  try {
    if (typeof value !== 'string') {
      return defaultValue
    }
    const parsed: unknown = JSON.parse(value)

    if (predicate != null) {
      if (predicate instanceof ZodSchema) {
        return predicate.parse(parsed)
      }

      if (predicate(parsed)) {
        return parsed
      }

      return defaultValue
    }

    // This is safe because if we don't pass a predicate,
    // we know that the parsed value is of type `T`.
    // eslint-disable-next-line no-restricted-syntax
    return parsed as T
  } catch {
    return defaultValue
  }
}
