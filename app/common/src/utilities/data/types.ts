import { isRef, type Ref } from 'vue'

/**
 * A value that is not an {@link Array}, or {@link object} unless it is a {@link Function} or Vue
 * {@link Ref}.
 */
export type ScalarOrRef =
  | undefined
  | number
  | string
  | boolean
  | bigint
  | symbol
  | null
  // eslint-disable-next-line @typescript-eslint/no-unsafe-function-type
  | Function
  | Ref

/**
 * Recurses into the provided value and applies the given function.
 *
 * Values entered are:
 * - Elements of arrays
 * - String-keyed values of any object that is not a {@link Function} or {@link Ref}.
 * All other values will be passed to the provided function.
 */
export function deepInspect(value: unknown, f: (scalar: ScalarOrRef) => void) {
  const recurse = (value: unknown) => {
    if (typeof value === 'undefined') f(value)
    else if (Array.isArray(value)) value.forEach(recurse)
    else if (typeof value === 'function') f(value)
    else if (typeof value === 'object') {
      if (value === null) f(value)
      else if (isRef(value)) f(value)
      else Object.values(value as Record<string, unknown>).forEach(recurse)
    } else if (
      typeof value === 'string' ||
      typeof value === 'number' ||
      typeof value === 'boolean' ||
      typeof value === 'bigint' ||
      typeof value === 'symbol' ||
      typeof value === 'function'
    )
      f(value)
    // TS seems confused here, value is narrowed to `{}` which is obviously wrong
    else f(value as never)
  }
  recurse(value)
}
