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
    .map(([k, v]: [k: string, v: unknown]) => {
      if (v === true) {
        return `${k}`
      }
      if (typeof v === 'string') {
        return `${k}=${JSON.stringify(v)}`
      }
      if (typeof v === 'function') {
        return `${k}={/* function */}`
      }
      if (typeof v === 'object' && v != null && Object.hasOwn(v, 'toString')) {
        // eslint-disable-next-line @typescript-eslint/no-base-to-string
        return `${k}={${String(v)}}`
      }
      return `${k}={${JSON.stringify(v)}}`
    })
    .join(' ')
}
