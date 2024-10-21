/**
 * @file
 *
 * A hook for copying text to the clipboard.
 */

import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as toastify from 'react-toastify'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

/** Props for the useCopy hook. */
export interface UseCopyProps {
  readonly copyText: string
  readonly onCopy?: () => void
  readonly successToastMessage?: boolean | string
}

const DEFAULT_TIMEOUT = 2000

/** A hook for copying text to the clipboard. */
export function useCopy(props: UseCopyProps) {
  const { copyText, onCopy, successToastMessage = true } = props

  const resetTimeoutIdRef = React.useRef<ReturnType<typeof setTimeout> | null>(null)
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  const copyQuery = reactQuery.useMutation({
    mutationFn: () => {
      return navigator.clipboard.writeText(copyText)
    },
    onMutate: () => {
      // Clear the reset timeout.
      // This is necessary to prevent the button from resetting while the copy is in progress.
      // This can happen if the user clicks the button multiple times in quick succession.
      if (resetTimeoutIdRef.current != null) {
        clearTimeout(resetTimeoutIdRef.current)
        resetTimeoutIdRef.current = null
      }
    },
    onSuccess: () => {
      onCopy?.()

      const toastId = 'copySuccess'

      if (successToastMessage !== false) {
        toastify.toast.success(
          successToastMessage === true ? getText('copiedToClipboard') : successToastMessage,
          { toastId, closeOnClick: true, hideProgressBar: true, position: 'bottom-right' },
        )
        // If user closes the toast, reset the button state
        toastify.toast.onChange((toast) => {
          if (toast.id === toastId && toast.status === 'removed') {
            copyQuery.reset()
          }
        })
      }

      // Reset the button to its original state after a timeout.
      resetTimeoutIdRef.current = setTimeout(() => {
        toastify.toast.dismiss(toastId)
        copyQuery.reset()
      }, DEFAULT_TIMEOUT)
    },
    onError: (error) => {
      toastAndLog('arbitraryErrorTitle', error)
    },
  })

  return copyQuery
}
