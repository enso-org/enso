/** @file Utilities related to modals. */
import { useImperativeHandle, useState, type ForwardedRef } from 'react'

/** Imperative API for modals. */
export interface ModalApi {
  readonly open: () => void
  readonly close: () => void
}

/** Handle a {@link ForwardedRef} for {@link ModalApi}, and return open state. */
export function useModalState(ref: ForwardedRef<ModalApi>) {
  const [isOpen, setIsOpen] = useState(false)

  useImperativeHandle(ref, () => ({
    open: () => {
      setIsOpen(true)
    },
    close: () => {
      setIsOpen(false)
    },
  }))

  return { isOpen, setIsOpen }
}
