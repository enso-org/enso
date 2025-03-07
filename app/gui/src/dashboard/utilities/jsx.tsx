/** @file Utilities related to JSX. */

/**
 * Convert an object to its JSX representation.
 * Converts primitives to strings directly.
 */
export function stringifyJsx(value: unknown) {
  if (typeof value !== 'object' || value == null) {
    return String(value)
  }
  return Object.entries(value)
    .flatMap(([k, v]) => {
      if (v === true) {
        return [`${k}`]
      }
      if (v === false) {
        return []
      }
      if (typeof v === 'string') {
        return [`${k}=${JSON.stringify(v)}`]
      }
      return [`${k}={${JSON.stringify(v)}}`]
    })
    .join(' ')
}
