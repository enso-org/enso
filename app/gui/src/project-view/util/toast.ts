import type { ResultError } from '@/util/data/result'
import { uuidv4 } from 'lib0/random'
// We are using `react-toastify`, since we share toast environment with dashboard.
import { toast, type ToastContent, type ToastOptions, type TypeOptions } from 'react-toastify'
import { onScopeDispose } from 'vue'

declare const toastIdBrand: unique symbol
type ToastId = string & { [toastIdBrand]: never }

function makeToastId(): ToastId {
  return `toast-${uuidv4()}` as ToastId
}

export interface UseToastOptions extends ToastOptions {
  outliveScope?: boolean
}

/**
 * Composable for new toast - a pop-up message displayed to the user.
 *
 * ```ts
 * // useToast.error is an equivalent of useToast(type: 'error').
 * // There's also useToast.info, useToast.warning and useToast.success.
 * const toastLspError = useToast.error()
 * // Every `useToast` allow displaying only one message at once, so
 * // here we create separate toast for every "topic".
 * const toastExecutionFailed = useToast.error()
 * const toastUserActionFailed = useToast.error()
 * // Toast are automatically closed after some time. Here we suppress this.
 * const toastStartup = useToast.info({ autoClose: false })
 * const toastConnectionLost = useToast.error({ autoClose: false })
 *
 * ```
 *
 * For details, read about `toastify` library.
 */
export function useToast(options: UseToastOptions = {}) {
  const id = makeToastId()
  if (options?.outliveScope !== true) {
    onScopeDispose(() => toast.dismiss(id))
  }

  return {
    /** Show or update toast. */
    show(content: ToastContent) {
      if (toast.isActive(id)) toast.update(id, { ...options, render: content })
      else toast(content, { ...options, toastId: id })
    },
    /** A helper for reporting {@link ResultError} to both toast and console. */
    reportError<E>(result: ResultError<E>, preamble?: string) {
      const msg = result.message(preamble)
      console.error(msg)
      this.show(msg)
    },
    /** Dismiss the displayed toast. */
    dismiss() {
      toast.dismiss(id)
    },
  }
}

const useToastKind = (type: TypeOptions) => (options?: UseToastOptions) =>
  useToast({ ...options, type })

useToast.error = useToastKind('error')
useToast.info = useToastKind('info')
useToast.warning = useToastKind('warning')
useToast.success = useToastKind('success')
