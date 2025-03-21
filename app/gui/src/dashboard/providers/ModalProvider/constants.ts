/** @file Constants for `ModalProvider`. */

import { createContext, type Dispatch, type RefObject, type SetStateAction } from 'react'
import { createStore } from 'zustand'

/** The type of a modal. */
export type Modal = JSX.Element

/**
 * A modal or a function that returns a modal.
 *
 * If a function is provided, it will be called with the previous modal as an argument,
 * and the return value will become the new modal.
 */
export type ModalOrCallback = Modal | ((prevModal: Modal | null) => Modal | null)

/** State contained in a `ModalStaticContext`. */
interface ModalStaticContextType {
  readonly setModal: Dispatch<SetStateAction<Modal | null>>
  readonly modalRef: RefObject<Modal>
}

/** State contained in a `ModalContext`. */
interface ModalContextType {
  readonly key: number
  readonly modal: Modal | null
}

export const ModalsStore = createStore<{
  readonly key: number
  readonly modal: Modal | null
  readonly setModal: (modal: ModalOrCallback | null) => void
  readonly updateModal: (modal: ModalOrCallback) => void
}>((set, get) => ({
  key: 0,
  modal: null,
  setModal: (modal) => {
    const existingModal = get().modal

    const nextKey = get().key + 1

    if (typeof modal === 'function') {
      set({ modal: modal(existingModal), key: nextKey })
    } else {
      set({ modal, key: nextKey })
    }
  },
  updateModal: (modal) => {
    const existingModal = get().modal

    if (existingModal == null) {
      throw new Error('Calling updateModal while no modal is set is forbidden.')
    }

    if (typeof modal === 'function') {
      set({ modal: modal(existingModal) })
    } else {
      set({ modal })
    }
  },
}))

export const ModalContext = createContext<ModalContextType>({ modal: null, key: 0 })

export const ModalStaticContext = createContext<ModalStaticContextType>({
  setModal: ModalsStore.getState().setModal,
  modalRef: {
    /**
     * Get the currently active modal.
     */
    get current() {
      return ModalsStore.getState().modal
    },
  },
})
