/** @file Contains useful error types common across the module. */

// =====================
// === tryGetMessage ===
// =====================

import * as toastify from 'react-toastify'

import * as loggerProvider from './providers/logger'

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

/** Like {@link tryGetMessage} but return the string representation of the value if it is not an
 * {@link Error} */
export function getMessageOrToString<T>(error: unknown) {
    return tryGetMessage(error) ?? String(error)
}

/** Return a toastify option object that renders an error message. */
// eslint-disable-next-line no-restricted-syntax
export function render(f: (message: string) => string): toastify.UpdateOptions {
    return { render: ({ data }) => f(getMessageOrToString(data)) }
}

/** Send a toast with rendered error message. Same message is logged as an error.
 *
 * @param messagePrefix - a prefix to add to the error message, should represent the immediate
 * error context.
 * @param error - the error to render, which will be appended to the message prefix.
 * @param options - additional options to pass to the toast API.
 * @returns - the toast ID. */
export function toastAndLog(
    messagePrefix: string,
    error?: unknown,
    options?: toastify.ToastOptions
) {
    const message =
        error == null ? `${messagePrefix}.` : `${messagePrefix}: ${getMessageOrToString(error)}`
    const id = toastify.toast.error(message, options)
    loggerProvider.useLogger().error(message)
    return id
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
