/** @file  */
import * as react from 'react'

export type Modal = () => JSX.Element

export interface ModalContextType {
    modal: Modal | null
    setModal: (modal: Modal | null) => void
}

// This is just the default value. It will never be used as
// eslint-disable-next-line @typescript-eslint/no-empty-function
const ModalContext = react.createContext<ModalContextType>({ modal: null, setModal: () => {} })

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
