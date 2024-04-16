/** Returns an all the keys of a type. The argument provided is required to be an object containing all the keys of the
 *  type (including optional fields), but the associated values are ignored and may be of any type. */
export function allKeys<T>(keys: { [P in keyof T]-?: any }): ReadonlySet<string> {
  return Object.freeze(new Set(Object.keys(keys)))
}

/** Static check that type `U` extends type `T`. */
export function mustExtend<T, U extends T>() {} // eslint-disable-line @typescript-eslint/no-unused-vars
