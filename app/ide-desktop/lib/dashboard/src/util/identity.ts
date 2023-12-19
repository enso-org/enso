/** @file An identity function. Useful to make TypeScript avoid narrowing. */

/** An identity function. */
export function identity<T>(value: T): T {
    return value
}
