/**
 * @file
 *
 * A hook for copying text to the clipboard.
 */

import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

import * as sonner from '#/components/Toast'

/**
 * Props for the useCopy hook.
 */
export interface UseCopyProps {
  readonly copyText: string
  readonly onCopy?: () => void
  readonly successToastMessage?: boolean | string
}

/**
 * A hook for copying text to the clipboard.
 */
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
        sonner.toast.success(
          successToastMessage === true ? getText('copiedToClipboard') : successToastMessage,
          {
            toastId,
            onAutoClose: copyQuery.reset,
            onDismiss: copyQuery.reset,
          }
        )
      }
    },
    onError: error => {
      toastAndLog('arbitraryErrorTitle', error)
    },
  })

  return copyQuery
}
