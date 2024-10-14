/**
 * @file
 *
 * A utility function to safely parse a JSON string.
 * returns the default value if the JSON string is invalid.
 * Also provides a type for the parsed JSON.
 */

/**
 * Safely parse a JSON string.
 * Parse the JSON string and return the default value if the JSON string is invalid.
 * Or if the parsed JSON does not match the type assertion.
 */
export function safeJsonParse<T = unknown>(
  value: string,
  defaultValue: T,
  predicate: (parsed: unknown) => parsed is T,
): T {
  try {
    const parsed: unknown = JSON.parse(value)
    return predicate(parsed) ? parsed : defaultValue
  } catch {
    return defaultValue
  }
}
