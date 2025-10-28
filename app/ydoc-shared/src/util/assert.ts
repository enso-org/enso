/* eslint-disable @typescript-eslint/no-unsafe-function-type */

/**
 * Assert that the current branch should be unreachable.
 * This function should never be called at runtime due to its parameter being of type `never`.
 * Being a type with zero values, it is impossible to construct an instance of this type at
 * runtime.
 */
export function assertNever(x: never): never {
  bail('Unexpected object: ' + JSON.stringify(x), assertNever)
}

/**
 * A type assertion that a condition is `true`.
 * Throw an error if the condtion is `false`.
 */
export function assert(
  condition: boolean,
  message?: string,
  constructorOpt: Function = assert,
): asserts condition {
  if (!condition)
    bail(message ? `Assertion failed: ${message}` : 'Assertion failed', constructorOpt)
}

/**
 * Checks if the given iterable has the specified length and throws an assertion error
 * if the lengths do not match.
 *export function assert
 * @param iterable The iterable to check.
 * @param length The expected length of the iterable.
 * @param message Optional message for the assertion error.
 * @throws Error Will throw an error if the length does not match.
 *
 * The first five elements of the iterable will be displayed in the error message
 * if the assertion fails. If the iterable contains more than five elements,
 * the remaining elements will be represented as '...'.
 */
export function assertLength<T>(
  iterable: Iterable<T>,
  length: number,
  message?: string,
  constructorOpt: Function = assertLength,
): void {
  const convertedArray = Array.from(iterable)
  const messagePrefix = message ? message + ' ' : ''
  const elementRepresentation =
    convertedArray.length > 5 ?
      `${convertedArray.slice(0, 5).join(', ')},...`
    : convertedArray.join(', ')
  assert(
    convertedArray.length === length,
    `${messagePrefix}Expected iterable of length ${length}, got length ${convertedArray.length}. Elements: [${elementRepresentation}]`,
    constructorOpt,
  )
}

/** Assert that an iterable contains zero elements. */
export function assertEmpty<T>(iterable: Iterable<T>, message?: string): void {
  assertLength(iterable, 0, message, assertEmpty)
}

/** Assert that two values are equal (by reference for reference types, by value for value types). */
export function assertEqual<T>(actual: T, expected: T, message?: string) {
  const messagePrefix = message ? message + ' ' : ''
  assert(actual === expected, `${messagePrefix}Expected ${expected}, got ${actual}.`, assertEqual)
}

/** Assert that two values are not equal (by reference for reference types, by value for value types). */
export function assertNotEqual<T>(actual: T, unexpected: T, message?: string) {
  const messagePrefix = message ? message + ' ' : ''
  assert(
    actual !== unexpected,
    `${messagePrefix}Expected not ${unexpected}, got ${actual}.`,
    assertNotEqual,
  )
}

/** A type assertion that a given value is not `undefined`. */
export function assertDefined<T>(x: T | undefined, message?: string): asserts x is T {
  const messagePrefix = message ? message + ' ' : ''
  assert(x !== undefined, `${messagePrefix}Expected value to be defined.`, assertDefined)
}

/** Assert that this case is unreachable. */
export function assertUnreachable(): never {
  bail('Unreachable code', assertUnreachable)
}

/**
 * Throw an error with provided message.
 *
 * It is convenient to use at the end of a nullable chain:
 * ```ts
 * const x = foo?.bar.baz?.() ?? bail('Expected foo.bar.baz to exist')
 * ```
 */
export function bail(message: string, constructorOpt: Function = bail): never {
  const error = new Error(message)
  if ('captureStackTrace' in Error) {
    Error.captureStackTrace(error, constructorOpt)
  }
  throw error
}
