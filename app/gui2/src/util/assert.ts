export function assertNever(x: never): never {
  bail('Unexpected object: ' + JSON.stringify(x))
}

export function assert(condition: boolean, message?: string): asserts condition {
  if (!condition) bail(message ? `Assertion failed: ${message}` : 'Assertion failed')
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
