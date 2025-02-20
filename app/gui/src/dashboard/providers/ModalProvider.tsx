/**
 * @file The React provider for modals, along with hooks to use the provider via
 * the shared React context.
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import * as React from 'react'

// =====================
// === ModalProvider ===
// =====================

/** The type of a modal. */
export type Modal = React.JSX.Element

/** State contained in a `ModalStaticContext`. */
interface ModalStaticContextType {
  readonly setModal: React.Dispatch<React.SetStateAction<Modal | null>>
  readonly modalRef: React.RefObject<Modal>
}

/** State contained in a `ModalContext`. */
interface ModalContextType {
  readonly key: number
  readonly modal: Modal | null
}

const ModalContext = React.createContext<ModalContextType>({ modal: null, key: 0 })

const ModalStaticContext = React.createContext<ModalStaticContextType>({
  setModal: () => {
    // Ignored. This default value will never be used as `ModalProvider` always provides
    // its own value.
  },
  modalRef: { current: null },
})

/** Props for a {@link ModalProvider}. */
export type ModalProviderProps = Readonly<React.PropsWithChildren>

/** A React provider containing the currently active modal. */
export default function ModalProvider(props: ModalProviderProps) {
  const { children } = props
  const [modal, setModal] = React.useState<Modal | null>(null)
  // We use keys to tell react to invalidate the DialogTrigger when we change the modal.
  const [key, setKey] = React.useState(0)
  const modalRef = useSyncRef(modal)

  const setModalStableCallback = useEventCallback(
    (nextModal: React.SetStateAction<React.JSX.Element | null>) => {
      React.startTransition(() => {
        setModal(nextModal)
        setKey((currentKey) => currentKey + 1)
      })
    },
  )

  // This is NOT for optimization purposes - this is for debugging purposes,
  // so that a change of `modal` does not trigger VDOM changes everywhere in the page.
  const setModalProvider = React.useMemo(
    () => (
      <ModalStaticProvider setModal={setModalStableCallback} modalRef={modalRef}>
        {children}
      </ModalStaticProvider>
    ),
    [children, modalRef, setModalStableCallback],
  )

  return <ModalContext.Provider value={{ modal, key }}>{setModalProvider}</ModalContext.Provider>
}

/** Props for a {@link ModalStaticProvider}. */
interface InternalModalStaticProviderProps extends Readonly<React.PropsWithChildren> {
  readonly setModal: React.Dispatch<React.SetStateAction<Modal | null>>
  readonly modalRef: React.RefObject<Modal>
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

// ================
// === useModal ===
// ================

/** A React context hook exposing the currently active modal, if one is currently visible. */
export function useModal() {
  const { modal, key } = React.useContext(ModalContext)
  return { modal, key } as const
}

// ===================
// === useModalRef ===
// ===================

/** A React context hook exposing the currently active modal (if one is currently visible) as a ref. */
export function useModalRef() {
  const { modalRef } = React.useContext(ModalStaticContext)
  return { modalRef } as const
}

// ===================
// === useSetModal ===
// ===================

/** A React context hook exposing functions to set and unset the currently active modal. */
export function useSetModal() {
  const { setModal: setModalRaw } = React.useContext(ModalStaticContext)

  const setModal = useEventCallback(setModalRaw)
  const updateModal = useEventCallback(setModalRaw)

  const unsetModal = useEventCallback(() => {
    setModalRaw(null)
  })

  return { setModal, updateModal, unsetModal } as const
}
