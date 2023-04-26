/** @file  */
import * as react from 'react'

import * as cloudService from '../dashboard/cloudService'
import * as localService from '../dashboard/localService'

export interface BackendContextType {
    backend: cloudService.Backend | localService.Backend
    setBackend: (backend: cloudService.Backend | localService.Backend) => void
}

// @ts-expect-error The default value will never be exposed
// as `backend` will always be accessed using `useBackend`.
const BackendContext = react.createContext<BackendContextType>(null)

// React components should always have a sibling `Props` interface
// if they accept props.
// eslint-disable-next-line @typescript-eslint/no-empty-interface
export interface BackendProviderProps extends React.PropsWithChildren<object> {
    initialBackend: cloudService.Backend | localService.Backend
}

export function BackendProvider(props: BackendProviderProps) {
    const { initialBackend, children } = props
    const [backend, setBackend] = react.useState<cloudService.Backend | localService.Backend>(
        initialBackend
    )
    return (
        <BackendContext.Provider value={{ backend, setBackend }}>
            {children}
        </BackendContext.Provider>
    )
}

export function useBackend() {
    const { backend } = react.useContext(BackendContext)
    return { backend }
}

export function useSetBackend() {
    const { setBackend } = react.useContext(BackendContext)
    return { setBackend }
}
