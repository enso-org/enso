/** @file Hooks for `ModalProvider`. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useContext } from 'react'
import { ModalContext, ModalStaticContext } from './constants'

/** A React context hook exposing the currently active modal, if one is currently visible. */
export function useModal() {
  const { modal, key } = useContext(ModalContext)
  return { modal, key } as const
}

/** A React context hook exposing the currently active modal (if one is currently visible) as a ref. */
export function useModalRef() {
  const { modalRef } = useContext(ModalStaticContext)
  return { modalRef } as const
}

/** A React context hook exposing functions to set and unset the currently active modal. */
export function useSetModal() {
  const { setModal: setModalRaw } = useContext(ModalStaticContext)

  const setModal = setModalRaw
  const updateModal = setModalRaw
  const unsetModal = useEventCallback(() => {
    setModalRaw(null)
  })

  return { setModal, updateModal, unsetModal } as const
}
