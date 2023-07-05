/** @file Defines the React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context. */
import * as react from 'react'

import * as common from 'enso-common'

import * as localBackend from '../dashboard/localBackend'
import * as remoteBackend from '../dashboard/remoteBackend'

// =================
// === Constants ===
// =================

/** The `localStorage` key under which the type of the current backend is stored. */
export const BACKEND_TYPE_KEY = `${common.PRODUCT_NAME.toLowerCase()}-dashboard-backend-type`

// =============
// === Types ===
// =============

/** A type representing a backend API that may be of any type. */
export type AnyBackendAPI = localBackend.LocalBackend | remoteBackend.RemoteBackend

// ======================
// === BackendContext ===
// ======================

/** State contained in a `BackendContext`. */
export interface BackendContextType {
    backend: AnyBackendAPI
    setBackend: (backend: AnyBackendAPI) => void
    setBackendWithoutSavingType: (backend: AnyBackendAPI) => void
}

// @ts-expect-error The default value will never be exposed
// as `backend` will always be accessed using `useBackend`.
const BackendContext = react.createContext<BackendContextType>(null)

/** Props for a {@link BackendProvider}. */
export interface BackendProviderProps extends React.PropsWithChildren<object> {
    initialBackend: AnyBackendAPI
}

// =======================
// === BackendProvider ===
// =======================

/** A React Provider that lets components get and set the current backend. */
export function BackendProvider(props: BackendProviderProps) {
    const { children } = props
    const [backend, setBackendWithoutSavingType] = react.useState<
        localBackend.LocalBackend | remoteBackend.RemoteBackend
        // This default value is UNSAFE, but must neither be `LocalBackend`, which may not be
        // available, not `RemoteBackend`, which does not work when not yet logged in.
        // Care must be taken to initialize the backend before its first usage.
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    >(null!)
    const setBackend = react.useCallback((newBackend: AnyBackendAPI) => {
        setBackendWithoutSavingType(newBackend)
        localStorage.setItem(BACKEND_TYPE_KEY, newBackend.type)
    }, [])

    return (
        <BackendContext.Provider value={{ backend, setBackend, setBackendWithoutSavingType }}>
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
    const { setBackend, setBackendWithoutSavingType } = react.useContext(BackendContext)
    return { setBackend, setBackendWithoutSavingType }
}
