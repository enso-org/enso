/** @file  */
import * as react from 'react'

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

const ModalContext = react.createContext<ModalContextType>({
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
    const [modal, setModal] = react.useState<Modal | null>(null)
    return <ModalContext.Provider value={{ modal, setModal }}>{children}</ModalContext.Provider>
}

/** A React context hook exposing the currently active modal, if one is currently visible. */
export function useModal() {
    const { modal } = react.useContext(ModalContext)
    return { modal }
}

/** A React context hook exposing functions to set and unset the currently active modal. */
export function useSetModal() {
    const { setModal: setModalRaw } = react.useContext(ModalContext)
    const setModal = (modal: Modal) => {
        setModalRaw(modal)
    }
    const unsetModal = () => {
        setModalRaw(null)
    }
    return { setModal, unsetModal }
}
