/**
 * @file The React provider for modals, along with hooks to use the provider via
 * the shared React context.
 */
import { useStore } from '#/hooks/storeHooks'
import * as React from 'react'
import { createStore } from 'zustand'

/** The type of a modal. */
export type Modal = React.JSX.Element

/**
 * A modal or a function that returns a modal.
 *
 * If a function is provided, it will be called with the previous modal as an argument,
 * and the return value will become the new modal.
 */
export type ModalOrCallback = Modal | ((prevModal: Modal | null) => Modal | null)

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

const ModalsStore = createStore<{
  readonly key: number
  readonly modal: Modal | null
  readonly setModal: (modal: ModalOrCallback | null) => void
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
}))

/**
 * Set the currently active modal.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function setModal(modal: ModalOrCallback) {
  const modalsStore = ModalsStore.getState()
  modalsStore.setModal(modal)
}

/**
 * Unset the currently active modal.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function unsetModal() {
  const modalsStore = ModalsStore.getState()
  if (modalsStore.modal != null) {
    modalsStore.setModal(null)
  } else {
    return false
  }
}

/**
 * Get the currently active modal.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function getModal() {
  const modalsStore = ModalsStore.getState()
  return modalsStore.modal
}

const ModalContext = React.createContext<ModalContextType>({ modal: null, key: 0 })

const ModalStaticContext = React.createContext<ModalStaticContextType>({
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

/** Props for a {@link ModalProvider}. */
export type ModalProviderProps = Readonly<React.PropsWithChildren>

/** A React provider containing the currently active modal. */
export default function ModalProvider(props: ModalProviderProps) {
  const { children } = props

  const modalState = useStore(ModalsStore, (state) => state, {
    areEqual: 'never',
    unsafeEnableTransition: true,
  })

  return (
    <ModalContext.Provider value={{ modal: modalState.modal, key: modalState.key }}>
      <ModalStaticProvider>{children}</ModalStaticProvider>
    </ModalContext.Provider>
  )
}

/** Props for a {@link ModalStaticProvider}. */
interface InternalModalStaticProviderProps extends Readonly<React.PropsWithChildren> {}

/** A React provider containing a function to set the currently active modal. */
function ModalStaticProvider(props: InternalModalStaticProviderProps) {
  const { children } = props

  const modalState = useStore(ModalsStore, (state) => ({ setModal: state.setModal }), {
    areEqual: 'always',
    unsafeEnableTransition: true,
  })

  const modalRef = useStore(ModalsStore, (state) => ({ current: state.modal }), {
    areEqual: 'object',
    unsafeEnableTransition: true,
  })

  return (
    <ModalStaticContext.Provider value={{ setModal: modalState.setModal, modalRef }}>
      {children}
    </ModalStaticContext.Provider>
  )
}

/** A React context hook exposing the currently active modal, if one is currently visible. */
// eslint-disable-next-line react-refresh/only-export-components
export function useModal() {
  const { modal, key } = React.useContext(ModalContext)
  return { modal, key } as const
}

/** A React context hook exposing the currently active modal (if one is currently visible) as a ref. */
// eslint-disable-next-line react-refresh/only-export-components
export function useModalRef() {
  const { modalRef } = React.useContext(ModalStaticContext)
  return { modalRef } as const
}
