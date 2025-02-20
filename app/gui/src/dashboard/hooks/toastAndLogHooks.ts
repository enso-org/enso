/** @file */
import * as toastify from 'react-toastify'

import type * as text from 'enso-common/src/text'

import * as loggerProvider from '#/providers/LoggerProvider'
import * as textProvider from '#/providers/TextProvider'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as errorModule from '#/utilities/error'

// ===========================
// === ToastAndLogCallback ===
// ===========================

/** The type of the `toastAndLog` function returned by {@link useToastAndLog}. */
export type ToastAndLogCallback = ReturnType<typeof useToastAndLog>

// ======================
// === useToastAndLog ===
// ======================

/**
 * Return a function to send a toast with rendered error message. The same message is also logged
 * as an error.
 */
export function useToastAndLogWithId() {
  const { getText } = textProvider.useText()
  const logger = loggerProvider.useLogger()

  return useEventCallback(
    <K extends text.TextId, T>(
      toastId: toastify.Id,
      textId: K | null,
      ...[error, ...replacements]: text.Replacements[K] extends readonly [] ?
        [error?: Error | errorModule.MustNotBeKnown<T>]
      : [error: Error | errorModule.MustNotBeKnown<T> | null, ...replacements: text.Replacements[K]]
    ) => {
      const messagePrefix =
        textId == null ? null
          // This is SAFE, as `replacements` is only `[]` if it was already `[]`.
          // See the above conditional type.
          // eslint-disable-next-line no-restricted-syntax
        : getText(textId, ...(replacements as text.Replacements[K]))
      const message =
        error == null ?
          `${messagePrefix ?? ''}.`
          // DO NOT explicitly pass the generic parameter anywhere else.
          // It is only being used here because this function also checks for
          // `MustNotBeKnown<T>`.
        : `${
            messagePrefix != null ? messagePrefix + ': ' : ''
          }${errorModule.getMessageOrToString<unknown>(error)}`
      toastify.toast.update(toastId, {
        type: 'error',
        render: message,
        isLoading: false,
        autoClose: null,
      })
      logger.error(message)
    },
  )
}

/**
 * Return a function to send a toast with rendered error message. The same message is also logged
 * as an error.
 */
export function useToastAndLog() {
  const { getText } = textProvider.useText()
  const logger = loggerProvider.useLogger()

  return useEventCallback(
    <K extends text.TextId, T>(
      textId: K | null,
      ...[error, ...replacements]: text.Replacements[K] extends readonly [] ?
        [error?: Error | errorModule.MustNotBeKnown<T>]
      : [error: Error | errorModule.MustNotBeKnown<T> | null, ...replacements: text.Replacements[K]]
    ) => {
      const messagePrefix =
        textId == null ? null
          // This is SAFE, as `replacements` is only `[]` if it was already `[]`.
          // See the above conditional type.
          // eslint-disable-next-line no-restricted-syntax
        : getText(textId, ...(replacements as text.Replacements[K]))
      const message =
        error == null ?
          `${messagePrefix ?? ''}.`
          // DO NOT explicitly pass the generic parameter anywhere else.
          // It is only being used here because this function also checks for
          // `MustNotBeKnown<T>`.
        : `${
            messagePrefix != null ? messagePrefix + ': ' : ''
          }${errorModule.getMessageOrToString<unknown>(error)}`
      const id = toastify.toast.error(message)
      logger.error(message)
      return id
    },
  )
}
