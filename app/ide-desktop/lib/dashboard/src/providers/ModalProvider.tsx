/** @file The React provider for modals, along with hooks to use the provider via
 * the shared React context. */
import * as React from 'react'

// =============
// === Modal ===
// =============

/** The type of a modal. */
export type Modal = JSX.Element

/** State contained in a `ModalStaticContext`. */
interface ModalStaticContextType {
  setModal: React.Dispatch<React.SetStateAction<Modal | null>>
  modalRef: React.RefObject<Modal>
}

/** State contained in a `ModalContext`. */
interface ModalContextType {
  modal: Modal | null
}

const ModalContext = React.createContext<ModalContextType>({
  modal: null,
})

const ModalStaticContext = React.createContext<ModalStaticContextType>({
  setModal: () => {
    // Ignored. This default value will never be used as `ModalProvider` always provides
    // its own value.
  },
  modalRef: { current: null },
})

/** Props for a {@link ModalProvider}. */
export interface ModalProviderProps extends React.PropsWithChildren {}

/** A React provider containing the currently active modal. */
export default function ModalProvider(props: ModalProviderProps) {
  const { children } = props
  const [modal, setModal] = React.useState<Modal | null>(null)
  const modalRef = React.useRef(modal)

  React.useEffect(() => {
    modalRef.current = modal
  }, [modal])

  // This is NOT for optimization purposes - this is for debugging purposes,
  // so that a change of `modal` does not trigger VDOM changes everywhere in the page.
  const setModalProvider = React.useMemo(
    () => (
      <ModalStaticProvider setModal={setModal} modalRef={modalRef}>
        {children}
      </ModalStaticProvider>
    ),
    [children]
  )
  return <ModalContext.Provider value={{ modal }}>{setModalProvider}</ModalContext.Provider>
}

/** Props for a {@link ModalProvider}. */
interface InternalModalStaticProviderProps extends React.PropsWithChildren {
  setModal: React.Dispatch<React.SetStateAction<Modal | null>>
  modalRef: React.RefObject<Modal>
}

/** A React provider containing a function to set the currently active modal. */
function ModalStaticProvider(props: InternalModalStaticProviderProps) {
  const { setModal, modalRef, children } = props

  return (
    <ModalStaticContext.Provider value={{ setModal, modalRef }}>
      {children}
    </ModalStaticContext.Provider>
  )
}

/** A React context hook exposing the currently active modal, if one is currently visible. */
export function useModal() {
  const { modal } = React.useContext(ModalContext)
  return { modal }
}

/** A React context hook exposing the currently active modal (if one is currently visible) as a ref.
 */
export function useModalRef() {
  const { modalRef } = React.useContext(ModalStaticContext)
  return { modalRef }
}

/** A React context hook exposing functions to set and unset the currently active modal. */
export function useSetModal() {
  const { setModal: setModalRaw } = React.useContext(ModalStaticContext)
  const setModal: (modal: Modal) => void = setModalRaw
  const updateModal: (updater: (modal: Modal | null) => Modal | null) => void = setModalRaw
  const unsetModal = React.useCallback(() => {
    setModalRaw(null)
  }, [/* should never change */ setModalRaw])
  return { setModal, updateModal, unsetModal }
}
