/** @file The React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context. */
import * as React from 'react'

import * as backendModule from '../dashboard/backend'
import * as localStorageModule from '../dashboard/localStorage'
import * as localStorageProvider from './localStorage'

// ======================
// === BackendContext ===
// ======================

/** State contained in a `BackendContext`. */
export interface BackendContextType {
    backend: backendModule.Backend
    setBackend: (backend: backendModule.Backend) => void
    setBackendWithoutSavingType: (backend: backendModule.Backend) => void
}

// @ts-expect-error The default value will never be exposed
// as `backend` will always be accessed using `useBackend`.
const BackendContext = React.createContext<BackendContextType>(null)

/** Props for a {@link BackendProvider}. */
export interface BackendProviderProps extends React.PropsWithChildren<object> {
    initialBackend: backendModule.Backend
}

// =======================
// === BackendProvider ===
// =======================

/** A React Provider that lets components get and set the current backend. */
export function BackendProvider(props: BackendProviderProps) {
    const { initialBackend, children } = props
    const { localStorage } = localStorageProvider.useLocalStorage()
    const [backend, setBackendWithoutSavingType] =
        React.useState<backendModule.Backend>(initialBackend)
    const setBackend = React.useCallback(
        (newBackend: backendModule.Backend) => {
            setBackendWithoutSavingType(newBackend)
            localStorage.set(localStorageModule.LocalStorageKey.backendType, newBackend.type)
        },
        [/* should never change */ localStorage]
    )

    return (
        <BackendContext.Provider value={{ backend, setBackend, setBackendWithoutSavingType }}>
            {children}
        </BackendContext.Provider>
    )
}

/** Exposes a property to get the current backend. */
export function useBackend() {
    const { backend } = React.useContext(BackendContext)
    return { backend }
}

/** Exposes a property to set the current backend. */
export function useSetBackend() {
    const { setBackend, setBackendWithoutSavingType } = React.useContext(BackendContext)
    return { setBackend, setBackendWithoutSavingType }
}
