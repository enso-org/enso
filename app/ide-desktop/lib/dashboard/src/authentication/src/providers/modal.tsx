/** @file  */
import * as React from 'react'

export type Modal = () => JSX.Element

export interface ModalContextType {
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

// React components should always have a sibling `Props` interface
// if they accept props.
// eslint-disable-next-line @typescript-eslint/no-empty-interface
export interface ModalProviderProps extends React.PropsWithChildren<object> {}

export function ModalProvider(props: ModalProviderProps) {
    const { children } = props
    const [modal, setModal] = React.useState<Modal | null>(null)
    return <ModalContext.Provider value={{ modal, setModal }}>{children}</ModalContext.Provider>
}

export function useModal() {
    const { modal } = React.useContext(ModalContext)
    return { modal }
}

export function useSetModal() {
    const { setModal } = React.useContext(ModalContext)
    const unsetModal = React.useCallback(() => {
        setModal(null)
    }, [setModal])
    return { setModal, unsetModal }
}
