/** @file Contains useful error types common across the module. */

// =====================
// === tryGetMessage ===
// =====================

/** Evaluates the given type only if it the exact same type as {@link Expected}. */
type MustBe<T, Expected> = (<U>() => U extends T ? 1 : 2) extends <U>() => U extends Expected
    ? 1
    : 2
    ? T
    : never

/** Used to enforce a parameter must be `any`. This is useful to verify that the value comes
 * from an API that returns `any`. */
type MustBeAny<T> = never extends T ? (0 extends T & 1 ? T : never) : never

export function tryGetMessage<T>(
    error: MustBe<T, object> | MustBe<T, unknown> | MustBeAny<T>
): string | null
/** Extracts the `message` property of a value if it is a string. Intended to be used on
 * {@link Error}s. */
export function tryGetMessage(error: unknown): string | null {
    return error != null &&
        typeof error === 'object' &&
        'message' in error &&
        typeof error.message === 'string'
        ? error.message
        : null
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
 * in an expresison.
 * @throws {UnreachableCaseError} Always. */
export function unreachable(value: never): never {
    throw new UnreachableCaseError(value)
}
