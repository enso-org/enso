/** @file Contains useful error types common across the module. */

// =====================
// === tryGetMessage ===
// =====================

/** Evaluates the given type only if it the exact same type as `Expected`. */
type MustBe<T, Expected> =
  (<U>() => U extends T ? 1 : 2) extends <U>() => U extends Expected ? 1 : 2 ? T : never

/** Used to enforce a parameter must be `any`. This is useful to verify that the value comes
 * from an API that returns `any`. */
type MustBeAny<T> = never extends T ? (0 extends T & 1 ? T : never) : never

/** Enforces that a parameter must not have a known type. This means the only types allowed are
 * `{}`, `object`, `unknown` and `any`. */
export type MustNotBeKnown<T> =
  // eslint-disable-next-line @typescript-eslint/ban-types, no-restricted-syntax
  MustBe<T, {}> | MustBe<T, object> | MustBe<T, unknown> | MustBeAny<T>

/** Extracts the `message` property of a value if it is a string. Intended to be used on
 * {@link Error}s. */
export function tryGetMessage<T, DefaultMessage extends string | null = null>(
  error: MustNotBeKnown<T>,
  // eslint-disable-next-line no-restricted-syntax
  defaultMessage: DefaultMessage = null as DefaultMessage
): DefaultMessage | string {
  const unknownError: unknown = error
  return unknownError != null &&
    typeof unknownError === 'object' &&
    'message' in unknownError &&
    typeof unknownError.message === 'string'
    ? unknownError.message
    : defaultMessage
}

/** Extracts the `error` property of a value if it is a string. */
export function tryGetError<T>(error: MustNotBeKnown<T>): string | null {
  const unknownError: unknown = error
  return unknownError != null &&
    typeof unknownError === 'object' &&
    'error' in unknownError &&
    typeof unknownError.error === 'string'
    ? unknownError.error
    : null
}

/**
 * Extracts the `stack` property of a value if it is a string. Intended to be used on {@link Error}s.
 */
export function tryGetStack<T, DefaultMessage extends string | null = null>(
  error: MustNotBeKnown<T>,
  // eslint-disable-next-line no-restricted-syntax
  defaultMessage: DefaultMessage = null as DefaultMessage
): DefaultMessage | string {
  const unknownError: unknown = error
  return unknownError != null &&
    typeof unknownError === 'object' &&
    'stack' in unknownError &&
    typeof unknownError.stack === 'string'
    ? unknownError.stack
    : defaultMessage
}

/** Like {@link tryGetMessage} but return the string representation of the value if it is not an
 * {@link Error}. */
export function getMessageOrToString<T>(error: MustNotBeKnown<T>) {
  return tryGetMessage(error) ?? String(error)
}

// ============================
// === UnreachableCaseError ===
// ============================

/** An error used to indicate when an unreachable case is hit in a `switch` or `if` statement.
 *
 * TypeScript is sometimes unable to determine if we're exhaustively matching in a `switch` or `if`
 * statement, so we introduce this error in the `default` case (or equivalent) to ensure that we at
 * least find out at runtime if we've missed a case, or forgotten to update the code when we add a
 * new case. */
export class UnreachableCaseError extends Error {
  /** Creates an `UnreachableCaseError`.
   * The parameter should be `never` since it is unreachable assuming all logic is sound. */
  constructor(value: never) {
    super(`Unreachable case: ${JSON.stringify(value)}`)
  }
}

/** A function that throws an {@link UnreachableCaseError} so that it can be used
 * in an expression.
 * @throws {UnreachableCaseError} Always. */
export function unreachable(value: never): never {
  throw new UnreachableCaseError(value)
}

// ==============
// === assert ===
// ==============

/** Assert that a value is truthy.
 * @throws {Error} when the value is not truthy. */
// These literals are REQUIRED, as they are falsy.
// eslint-disable-next-line @typescript-eslint/no-magic-numbers, no-restricted-syntax
export function assert<T>(makeValue: () => T | '' | 0 | 0n | false | null | undefined): T {
  const result = makeValue()
  // This function explicitly checks for truthiness.
  // eslint-disable-next-line @typescript-eslint/strict-boolean-expressions
  if (!result) {
    throw new Error(
      'Assertion failed: `' +
        makeValue.toString().replace(/^\s*[(].*?[)]\s*=>\s*/, '') +
        '` should not be `null`.'
    )
  } else {
    return result
  }
}

/**
 * Checks if the given error is a JavaScript execution error.
 */
export function isJSError(error: unknown): boolean {
  if (error instanceof TypeError) {
    return true
  } else if (error instanceof ReferenceError) {
    return true
  } else if (error instanceof SyntaxError) {
    return true
  } else if (error instanceof RangeError) {
    return true
  } else if (error instanceof URIError) {
    return true
  } else if (error instanceof EvalError) {
    return true
  } else {
    return false
  }
}
