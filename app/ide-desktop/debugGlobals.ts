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

// This is required to make the definition `Object.prototype.$d$` not error.
// eslint-disable-next-line no-restricted-syntax
declare global {
    // Documentation is already inherited.
    /** */
    interface Object {
        /** Log self and return self. */
        $d$: <T>(this: T, message?: string) => T
    }
}

if (!('$d$' in Object.prototype)) {
    Object.defineProperty(Object.prototype, '$d$', {
        /** Log self and return self. */
        value: function <T>(this: T, message?: string) {
            if (message != null) {
                console.log(message, this)
            } else {
                console.log(this)
            }
            return this
        },
        enumerable: false,
        writable: false,
        configurable: false,
    })
}
