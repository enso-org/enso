/**
 * Returns an all the keys of a type. The argument provided is required to be an object containing all the keys of the
 *  type (including optional fields), but the associated values are ignored and may be of any type.
 */
export function allKeys<T>(keys: { [P in keyof T]-?: any }): ReadonlySet<string> {
  return Object.freeze(new Set(Object.keys(keys)))
}

/** Static check that type `T` extends type `U`. */
export function mustExtend<T extends U, U>() {} // eslint-disable-line @typescript-eslint/no-unused-vars

/**
 * A transformation of function's generic parameter `T` to a type which will forbid properties
 * not belonging to `S` if object literal is passed.
 *
 * Example:
 * ```js
 * interface Options {
 *   a?: boolean
 *   b?: number
 * }
 *
 * function specifyOptions<T extends Options>(x: ForbidExcessProps<T, Options>): T {
 *   // something something
 *   return x
 * }
 *
 * // This works, and type system knows that `a` exists in `works`
 * const works = specifyOptions({ a: true })
 * // But this still raises a compile error
 * const dont = specifyOptions({ c: true })
 * ```
 */
export type ForbidExcessProps<T, S> = { [K in keyof T]: K extends keyof S ? T[K] : never }

/**
 * An arbitrary class type that doesn't necessarily provide a public constructor.
 * Works with standard and abstract classes.
 */
// eslint-disable-next-line @typescript-eslint/no-unsafe-function-type
export type Class<T> = Function & {
  prototype: T
}

/** Check if the the code runs in the headless environment. */
export function isHeadless() {
  return typeof window === 'undefined'
}
