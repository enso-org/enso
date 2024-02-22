/** @file Contains useful error types common across the module. */

// =====================
// === tryGetMessage ===
// =====================

/** Evaluates to the given type only if it the exact same type as `Expected`. */
type MustBe<T, Expected> = (<U>() => U extends T ? 1 : 2) extends <U>() => U extends Expected
  ? 1
  : 2
  ? T
  : never

/** Used to enforce a parameter must be `any`. This is useful to verify that the value comes
 * from an API that returns `any`. */
type MustBeAny<T> = never extends T ? (0 extends T & 1 ? T : never) : never

/** Enforces that a parameter must not have a known type. This means the only types allowed are
 * `{}`, `object`, `unknown` and `any`. */
export type MustNotBeKnown<T> =
  // eslint-disable-next-line @typescript-eslint/ban-types, no-restricted-syntax
  MustBe<T, {}> | MustBe<T, object> | MustBe<T, unknown> | MustBeAny<T>

/** Extract the `message` property of a value if it is a string. Intended to be used on
 * {@link Error}s. */
export function tryGetMessage<T>(error: MustNotBeKnown<T>): string | null {
  const unknownError: unknown = error
  return unknownError != null &&
    typeof unknownError === 'object' &&
    'message' in unknownError &&
    typeof unknownError.message === 'string'
    ? unknownError.message
    : null
}
