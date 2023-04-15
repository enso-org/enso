/** @file  */
import * as react from 'react'

export type Modal = () => JSX.Element

export interface ModalContextType {
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

// React components should always have a sibling `Props` interface
// if they accept props.
// eslint-disable-next-line @typescript-eslint/no-empty-interface
export interface ModalProviderProps extends React.PropsWithChildren<object> {}

export function ModalProvider(props: ModalProviderProps) {
    const { children } = props
    const [modal, setModal] = react.useState<Modal | null>(null)
    return <ModalContext.Provider value={{ modal, setModal }}>{children}</ModalContext.Provider>
}

export function useModal() {
    const { modal } = react.useContext(ModalContext)
    return { modal }
}

export function useSetModal() {
    const { setModal } = react.useContext(ModalContext)
    function unsetModal() {
        setModal(null)
    }
    return { setModal, unsetModal }
}
