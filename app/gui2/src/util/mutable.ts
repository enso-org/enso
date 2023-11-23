/** Removes `readonly` from all keys in a type. UNSAFE. */
export type UnsafeMutable<T> = { -readonly [K in keyof T]: T[K] }
