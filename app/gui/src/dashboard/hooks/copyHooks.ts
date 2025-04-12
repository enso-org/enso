/**
 * @file
 *
 * A hook for copying text to the clipboard.
 */

import * as React from 'react'

import * as toastify from 'react-toastify'

import * as textProvider from '#/providers/TextProvider'
import { useEventCallback } from './eventCallbackHooks'

/** Props for the useCopy hook. */
export interface UseCopyProps {
  readonly onCopy?: (() => void) | undefined
  readonly successToastMessage?: boolean | string
}

const DEFAULT_TIMEOUT = 2000

/** A hook for copying text to the clipboard. */
export function useCopy(props: UseCopyProps = {}) {
  const { onCopy, successToastMessage = true } = props

  const resetTimeoutIdRef = React.useRef<ReturnType<typeof setTimeout> | null>(null)
  const { getText } = textProvider.useText()

  const [isCopying, startTransition] = React.useTransition()
  const [isCopied, setIsCopied] = React.useOptimistic(false)

  const copy = useEventCallback(
    (text: string) =>
      new Promise<void>((resolve) => {
        startTransition(async () => {
          await navigator.clipboard.writeText(text)
          setIsCopied(true)
          onCopy?.()

          const toastId = 'copySuccess'

          if (successToastMessage !== false) {
            toastify.toast.success(
              successToastMessage === true ? getText('copiedToClipboard') : successToastMessage,
              { toastId, closeOnClick: true, hideProgressBar: true, position: 'bottom-right' },
            )
          }

          await new Promise<void>((timeoutResolve) => {
            // Reset the button to its original state after a timeout.
            resetTimeoutIdRef.current = setTimeout(() => {
              toastify.toast.dismiss(toastId)
              timeoutResolve()
            }, DEFAULT_TIMEOUT)

            // If user closes the toast, reset the button state
            toastify.toast.onChange((toast) => {
              if (toast.id === toastId && toast.status === 'removed') {
                timeoutResolve()
              }
            })
          })

          resolve()
        })
      }),
  )

  return { copy, isCopying, isCopied }
}
