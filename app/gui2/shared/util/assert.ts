export function assertNever(x: never): never {
  bail('Unexpected object: ' + JSON.stringify(x))
}

export function assert(condition: boolean, message?: string): asserts condition {
  if (!condition) bail(message ? `Assertion failed: ${message}` : 'Assertion failed')
}

/**
 * Checks if the given iterable has the specified length and throws an assertion error
 * if the lengths do not match.
 *
 * @param iterable The iterable to check.
 * @param length The expected length of the iterable.
 * @param message Optional message for the assertion error.
 * @return void
 * @throws Error Will throw an error if the length does not match.
 *
 * The first five elements of the iterable will be displayed in the error message
 * if the assertion fails. If the iterable contains more than five elements,
 * the remaining elements will be represented as '...'.
 */
export function assertLength<T>(iterable: Iterable<T>, length: number, message?: string): void {
  const convertedArray = Array.from(iterable)
  const messagePrefix = message ? message + ' ' : ''
  const elementRepresentation =
    convertedArray.length > 5
      ? `${convertedArray.slice(0, 5).join(', ')},...`
      : convertedArray.join(', ')
  assert(
    convertedArray.length === length,
    `${messagePrefix}Expected iterable of length ${length}, got length ${convertedArray.length}. Elements: [${elementRepresentation}]`,
  )
}

export function assertEmpty<T>(iterable: Iterable<T>, message?: string): void {
  assertLength(iterable, 0, message)
}

export function assertEqual<T>(actual: T, expected: T, message?: string) {
  const messagePrefix = message ? message + ' ' : ''
  assert(actual === expected, `${messagePrefix}Expected ${expected}, got ${actual}.`)
}

export function assertNotEqual<T>(actual: T, unexpected: T, message?: string) {
  const messagePrefix = message ? message + ' ' : ''
  assert(actual !== unexpected, `${messagePrefix}Expected not ${unexpected}, got ${actual}.`)
}

export function assertDefined<T>(x: T | undefined, message?: string): asserts x is T {
  const messagePrefix = message ? message + ' ' : ''
  assert(x !== undefined, `${messagePrefix}Expected value to be defined.`)
}

export function assertUnreachable(): never {
  bail('Unreachable code')
}

/**
 * Throw an error with provided message.
 *
 * It is convenient to use at the end of a nullable chain:
 * ```ts
 * const x = foo?.bar.baz?.() ?? bail('Expected foo.bar.baz to exist')
 * ```
 */
export function bail(message: string): never {
  throw new Error(message)
}
