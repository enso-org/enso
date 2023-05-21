/** @file Debug-only functions, injected or stripped by the build tool as appropriate. */

/** The logger used by {@link assert}. */
interface Logger {
    error: (message: string) => void
}

/** Logs an error . */
export function assert(invariant: boolean, message: string, logger: Logger = console) {
    if (!invariant) {
        logger.error('assertion failed: ' + message)
    }
}
