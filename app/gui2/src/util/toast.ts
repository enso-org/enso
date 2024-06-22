import type { ResultError } from '@/util/data/result'
import { toast, type ToastKind, type ToastOptions } from 'enso-dashboard'
import { uuidv4 } from 'lib0/random'
import { onScopeDispose } from 'vue'

declare const toastIdBrand: unique symbol
type ToastId = string & { [toastIdBrand]: never }

function makeToastId(): ToastId {
  return `toast-${uuidv4()}` as ToastId
}

export interface UseToastOptions extends ToastOptions {
  outliveScope?: boolean
}

export function useToast(options: UseToastOptions = {}) {
  const id = makeToastId()
  if (options?.outliveScope !== true) {
    onScopeDispose(() => toast.dismiss(id))
  }

  return {
    show(content: string) {
      return toast.message(content, { ...options, toastId: id })
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

const useToastKind = (type: ToastKind) => (options?: UseToastOptions) =>
  useToast({ ...options, type })

useToast.error = useToastKind('error')
useToast.info = useToastKind('info')
useToast.warning = useToastKind('warning')
useToast.success = useToastKind('success')
