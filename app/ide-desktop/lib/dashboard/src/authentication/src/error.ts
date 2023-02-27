/** An error used to indicate when an unreachable case is hit in a `switch` or `if` statement.
 * 
 * TypeScript is sometimes unable to determine if we're exhaustively matching in a `switch` or `if`
 * statement, so we introduce this error in the `default` case (or equivalent) to ensure that we at
 * least find out at runtime if we've missed a case, or forgotten to update the code when we add a
 * new case. */
export class UnreachableCaseError extends Error {
    constructor(value: never) {
        // TypeScript doesn't let us use `never` values in literals, so we have to disable the
        // rule to allow this. As for why we're using `never` in a literal, it's because we expect
        // TypeScript to let us know that we're in an unreachable case at type check time (through
        // the signature of this constructor), but if that fails we still want to know at runtime.
        super(`Unreachable case: ${value}`)
    }
}
