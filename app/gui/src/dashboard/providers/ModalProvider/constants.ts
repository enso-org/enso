/** @file Constants for `ModalProvider`. */

import { createContext, type Dispatch, type RefObject, type SetStateAction } from 'react'

/** The type of a modal. */
export type Modal = JSX.Element

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

export const ModalContext = createContext<ModalContextType>({ modal: null, key: 0 })

export const ModalStaticContext = createContext<ModalStaticContextType>({
  setModal: () => {},
  modalRef: { current: null },
})
