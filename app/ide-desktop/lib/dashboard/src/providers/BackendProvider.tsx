/** @file The React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context. */
import * as React from 'react'

import * as localStorageProvider from '#/providers/LocalStorageProvider'

import type Backend from '#/services/Backend'

import * as localStorageModule from '#/utilities/LocalStorage'

// ======================
// === BackendContext ===
// ======================

/** State contained in a `BackendContext`. */
export interface BackendContextType {
  backend: Backend
  setBackend: (backend: Backend) => void
  setBackendWithoutSavingType: (backend: Backend) => void
}

// @ts-expect-error The default value will never be exposed
// as `backend` will always be accessed using `useBackend`.
const BackendContext = React.createContext<BackendContextType>(null)

/** Props for a {@link BackendProvider}. */
export interface BackendProviderProps extends React.PropsWithChildren<object> {
  initialBackend: Backend
}

// =======================
// === BackendProvider ===
// =======================

/** A React Provider that lets components get and set the current backend. */
export default function BackendProvider(props: BackendProviderProps) {
  const { initialBackend, children } = props
  const { localStorage } = localStorageProvider.useLocalStorage()
  const [backend, setBackendWithoutSavingType] = React.useState<Backend>(initialBackend)
  const setBackend = React.useCallback(
    (newBackend: Backend) => {
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
