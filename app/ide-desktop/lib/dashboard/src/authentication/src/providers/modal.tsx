/** @file The React provider for modals, along with hooks to use the provider via
 * the shared React context. */
import * as React from 'react'

// =============
// === Modal ===
// =============

/** The type of a modal. */
export type Modal = () => JSX.Element

/** State contained in a `ModalContext`. */
interface ModalContextType {
    modal: Modal | null
    setModal: (modal: Modal | null) => void
}

const ModalContext = React.createContext<ModalContextType>({
    modal: null,
    setModal: () => {
        // Ignored. This default value will never be used
        // as `ModalProvider` always provides its own value.
    },
})

/** Props for a {@link ModalProvider}. */
export interface ModalProviderProps extends React.PropsWithChildren<object> {}

/** A React provider containing the currently active modal. */
export function ModalProvider(props: ModalProviderProps) {
    const { children } = props
    const [modal, setModal] = React.useState<Modal | null>(null)
    return <ModalContext.Provider value={{ modal, setModal }}>{children}</ModalContext.Provider>
}

/** A React context hook exposing the currently active modal, if one is currently visible. */
export function useModal() {
    const { modal } = React.useContext(ModalContext)
    return { modal }
}

/** A React context hook exposing functions to set and unset the currently active modal. */
export function useSetModal() {
    const { setModal } = React.useContext(ModalContext)
    const unsetModal = React.useCallback(() => {
        setModal(null)
    }, [setModal])
    return { setModal, unsetModal }
}
