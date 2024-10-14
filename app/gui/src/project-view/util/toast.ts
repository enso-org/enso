// We are using `react-toastify`, since we share toast environment with dashboard.
import type { ResultError } from '@/util/data/result'
import { uuidv4 } from 'lib0/random'
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

/** TODO: Add docs */
export function useToast(options: UseToastOptions = {}) {
  const id = makeToastId()
  if (options?.outliveScope !== true) {
    onScopeDispose(() => toast.dismiss(id))
  }

  return {
    show(content: ToastContent) {
      if (toast.isActive(id)) toast.update(id, { ...options, render: content })
      else toast(content, { ...options, toastId: id })
    },
    reportError<E>(result: ResultError<E>, preamble?: string) {
      const msg = result.message(preamble)
      console.error(msg)
      this.show(msg)
    },
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
