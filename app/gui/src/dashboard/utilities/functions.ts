/** @file A collection of generic utility functions. */

/** A stable reference to a function that does nothing. */
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any
export const noop: (...args: any[]) => void = () => {}

/** A stable reference to a function that returns its input. */
export function identity<T>(value: T) {
  return value
}
