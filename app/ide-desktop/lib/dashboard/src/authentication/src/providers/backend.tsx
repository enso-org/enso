/** @file Defines the React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context. */
import * as react from 'react'

import * as cloudBackend from '../dashboard/remoteBackend'
import * as localBackend from '../dashboard/localBackend'

// =============
// === Types ===
// =============

/** A type representing a backend API that may be of any type. */
type AnyBackendAPI = cloudBackend.RemoteBackend | localBackend.LocalBackend

// ======================
// === BackendContext ===
// ======================

export interface BackendContextType {
    backend: AnyBackendAPI
    setBackend: (backend: AnyBackendAPI) => void
}

// @ts-expect-error The default value will never be exposed
// as `backend` will always be accessed using `useBackend`.
const BackendContext = react.createContext<BackendContextType>(null)

export interface BackendProviderProps extends React.PropsWithChildren<object> {
    initialBackend: AnyBackendAPI
}

// =======================
// === BackendProvider ===
// =======================

/** A React Provider that lets components get and set the current backend. */
export function BackendProvider(props: BackendProviderProps) {
    const { initialBackend, children } = props
    const [backend, setBackend] = react.useState<
        cloudBackend.RemoteBackend | localBackend.LocalBackend
    >(initialBackend)
    return (
        <BackendContext.Provider value={{ backend, setBackend }}>
            {children}
        </BackendContext.Provider>
    )
}

/** Exposes a property to get the current backend. */
export function useBackend() {
    const { backend } = react.useContext(BackendContext)
    return { backend }
}

/** Exposes a property to set the current backend. */
export function useSetBackend() {
    const { setBackend } = react.useContext(BackendContext)
    return { setBackend }
}
