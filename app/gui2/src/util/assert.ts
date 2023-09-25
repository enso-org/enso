export function assertNever(x: never): never {
  bail('Unexpected object: ' + x)
}

export function assert(condition: boolean): asserts condition {
  if (!condition) bail('Assertion failed')
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
