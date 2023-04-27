/** @file Contains useful error types common across the module. */

// ================================
// === Type assertions (unsafe) ===
// ================================

/** Used to enforce a parameter must be `any`. This is useful to verify that the value comes
 * from an API that returns `any`. */
type MustBeAny<T> = never extends T ? (T & 1 extends 0 ? T : never) : never

/** Assumes an unknown value is an {@link Error}. */
export function unsafeAsError<T>(error: MustBeAny<T>) {
    // This is UNSAFE - errors can be any value.
    // Usually they *do* extend `Error`,
    // however great care must be taken when deciding to use this.
    // eslint-disable-next-line no-restricted-syntax
    return error as Error
}

/** Extracts the `message` property of a value, by first assuming it is an {@link Error}. */
export function unsafeIntoErrorMessage<T>(error: MustBeAny<T>) {
    return unsafeAsError(error).message
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
