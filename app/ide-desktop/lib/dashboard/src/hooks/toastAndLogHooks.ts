/** @file */
import * as React from 'react'

import type * as text from '#/text'

import * as loggerProvider from '#/providers/LoggerProvider'
import * as textProvider from '#/providers/TextProvider'

import * as sonner from '#/components/Toast'

import * as errorModule from '#/utilities/error'

// ======================
// === useToastAndLog ===
// ======================

/** Return a function to send a toast with rendered error message. The same message is also logged
 * as an error. */
export function useToastAndLog() {
  const { getText } = textProvider.useText()
  const logger = loggerProvider.useLogger()

  return React.useCallback(
    <K extends text.TextId, T>(
      textId: K | null,
      ...[error, ...replacements]: text.Replacements[K] extends readonly []
        ? [error?: Error | errorModule.MustNotBeKnown<T>]
        : [
            error: Error | errorModule.MustNotBeKnown<T> | null,
            ...replacements: text.Replacements[K],
          ]
    ) => {
      const messagePrefix =
        textId == null
          ? null
          : // This is SAFE, as `replacements` is only `[]` if it was already `[]`.
            // See the above conditional type.
            // eslint-disable-next-line no-restricted-syntax
            getText(textId, ...(replacements as text.Replacements[K]))
      const message =
        error == null
          ? `${messagePrefix ?? ''}.`
          : // DO NOT explicitly pass the generic parameter anywhere else.
            // It is only being used here because this function also checks for
            // `MustNotBeKnown<T>`.
            `${
              messagePrefix != null ? messagePrefix + ': ' : ''
            }${errorModule.getMessageOrToString<unknown>(error)}`
      const id = sonner.toast.error(getText('arbitraryErrorTitle'), { description: message })
      logger.error(message)
      return id
    },
    [getText, logger]
  )
}
