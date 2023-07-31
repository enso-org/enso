/** @file The React provider for modals, along with hooks to use the provider via
 * the shared React context. */
import * as React from 'react'

// =============
// === Modal ===
// =============

/** The type of a modal. */
export type Modal = JSX.Element

/** State contained in a `SetModalContext`. */
interface SetModalContextType {
    setModal: React.Dispatch<React.SetStateAction<Modal | null>>
    setOnUnsetModal: React.Dispatch<React.SetStateAction<(() => void) | null>>
}

/** State contained in a `ModalContext`. */
interface ModalContextType {
    modal: Modal | null
}

const ModalContext = React.createContext<ModalContextType>({
    modal: null,
})

const SetModalContext = React.createContext<SetModalContextType>({
    setModal: () => {
        // Ignored. This default value will never be used
        // as `ModalProvider` always provides its own value.
    },
    setOnUnsetModal: () => {
        // Ignored.
    },
})

/** Props for a {@link ModalProvider}. */
export interface ModalProviderProps extends React.PropsWithChildren {}

/** A React provider containing the currently active modal. */
export function ModalProvider(props: ModalProviderProps) {
    const { children } = props
    const [modal, setModal] = React.useState<Modal | null>(null)
    const [, setOnUnsetModal] = React.useState<(() => void) | null>(null)
    // This is NOT for optimization purposes - this is for debugging purposes,
    // so that a change of `modal` does not trigger VDOM changes everywhere in the page.
    const setModalProvider = React.useMemo(
        () => (
            <SetModalProvider setModal={setModal} setOnUnsetModal={setOnUnsetModal}>
                {children}
            </SetModalProvider>
        ),
        [children]
    )
    return <ModalContext.Provider value={{ modal }}>{setModalProvider}</ModalContext.Provider>
}

/** Props for a {@link ModalProvider}. */
interface InternalSetModalProviderProps extends React.PropsWithChildren {
    setModal: React.Dispatch<React.SetStateAction<Modal | null>>
    setOnUnsetModal: React.Dispatch<React.SetStateAction<(() => void) | null>>
}

/** A React provider containing a function to set the currently active modal. */
function SetModalProvider(props: InternalSetModalProviderProps) {
    const { setModal, setOnUnsetModal, children } = props

    return (
        <SetModalContext.Provider value={{ setModal, setOnUnsetModal }}>
            {children}
        </SetModalContext.Provider>
    )
}

/** A React context hook exposing the currently active modal, if one is currently visible. */
export function useModal() {
    const { modal } = React.useContext(ModalContext)
    return { modal }
}

/** A React context hook exposing functions to set and unset the currently active modal. */
export function useSetModal() {
    const { setModal: setModalRaw, setOnUnsetModal } = React.useContext(SetModalContext)
    const setModal = React.useCallback(
        (modal: Modal, onUnsetModal?: () => void) => {
            setModalRaw(modal)
            setOnUnsetModal((oldOnUnsetModal: (() => void) | null) => {
                setTimeout(() => {
                    oldOnUnsetModal?.()
                }, 0)
                return onUnsetModal ?? null
            })
        },
        [/* should never change */ setModalRaw, /* should never change */ setOnUnsetModal]
    )
    const updateModal = React.useCallback(
        (updater: (modal: Modal | null) => Modal | null) => {
            setModalRaw(updater)
            setOnUnsetModal((oldOnUnsetModal: (() => void) | null) => {
                setTimeout(() => {
                    oldOnUnsetModal?.()
                }, 0)
                return null
            })
        },
        [/* should never change */ setModalRaw, /* should never change */ setOnUnsetModal]
    )
    const unsetModal = React.useCallback(() => {
        setModalRaw(null)
        setOnUnsetModal((oldOnUnsetModal: (() => void) | null) => {
            setTimeout(() => {
                oldOnUnsetModal?.()
            }, 0)
            return null
        })
    }, [/* should never change */ setModalRaw, /* should never change */ setOnUnsetModal])
    return { setModal, updateModal, unsetModal }
}
