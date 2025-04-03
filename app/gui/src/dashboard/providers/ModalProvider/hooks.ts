/** @file Hooks for `ModalProvider`. */
import { useContext } from 'react'
import { ModalContext, ModalsStore, ModalStaticContext, type ModalOrCallback } from './constants'

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

/**
 * A React context hook exposing functions to set and unset the currently active modal.
 * @deprecated Use directly imported `setModal`, `updateModal`, and `unsetModal` functions instead.
 */
export function useSetModal() {
  return { setModal, unsetModal } as const
}

/**
 * Set the currently active modal.
 * @throws An error if a modal is already set.
 */
export function setModal(modal: ModalOrCallback) {
  const modalsStore = ModalsStore.getState()
  modalsStore.setModal(modal)
}

/**
 * Unset the currently active modal.
 */
export function unsetModal() {
  const modalsStore = ModalsStore.getState()
  modalsStore.setModal(null)
}

/** Get the currently active modal. */
export function getModal() {
  const modalsStore = ModalsStore.getState()
  return modalsStore.modal
}
