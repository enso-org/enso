/** @file */
import * as React from 'react'

import * as toastify from 'react-toastify'

import * as loggerProvider from '#/providers/LoggerProvider'
import * as errorModule from '#/utilities/error'

// ======================
// === useToastAndLog ===
// ======================

/** Return a function to send a toast with rendered error message. The same message is also logged
 * as an error. */
export function useToastAndLog() {
    const logger = loggerProvider.useLogger()
    return React.useCallback(
        <T>(
            messagePrefix: string | null,
            error?: errorModule.MustNotBeKnown<T>,
            options?: toastify.ToastOptions
        ) => {
            const message =
                error == null
                    ? `${messagePrefix ?? ''}.`
                    : // DO NOT explicitly pass the generic parameter anywhere else.

                      // It is only being used here because this function also checks for
                      // `MustNotBeKnown<T>`.
                      `${
                          messagePrefix != null ? messagePrefix + ': ' : ''
                      }${errorModule.getMessageOrToString<unknown>(error)}`
            const id = toastify.toast.error(message, options)
            logger.error(message)
            return id
        },
        [/* should never change */ logger]
    )
}
