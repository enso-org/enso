export function assertNever(x: never): never {
  throw new Error('Unexpected object: ' + x)
}

export function assert(condition: boolean): asserts condition {
  if (!condition) throw new Error('Assertion failed')
}

export function assertUnreachable(): never {
  throw new Error('Unreachable code')
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
