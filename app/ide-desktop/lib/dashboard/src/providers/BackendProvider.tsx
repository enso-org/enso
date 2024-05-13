/** @file The React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context. */
import * as React from 'react'

import * as localStorageProvider from '#/providers/LocalStorageProvider'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as array from '#/utilities/array'
import LocalStorage from '#/utilities/LocalStorage'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly backendType: backendModule.BackendType
  }
}

const BACKEND_TYPES = Object.values(backendModule.BackendType)
LocalStorage.registerKey('backendType', {
  tryParse: value => (array.includes(BACKEND_TYPES, value) ? value : null),
})

// ======================
// === BackendContext ===
// ======================

/** State contained in a `BackendContext`. */
export interface BackendContextType {
  readonly backend: Backend
  readonly setBackend: (backend: Backend) => void
  readonly setBackendWithoutSavingType: (backend: Backend) => void
}

// as `backend` will always be accessed using `useBackend`.
const BackendContext = React.createContext<BackendContextType | null>(null)

/** Props for a {@link BackendProvider}. */
export interface BackendProviderProps extends Readonly<React.PropsWithChildren> {
  readonly initialBackend: Backend
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
      localStorage.set('backendType', newBackend.type)
    },
    [/* should never change */ localStorage]
  )

  return (
    <BackendContext.Provider value={{ backend, setBackend, setBackendWithoutSavingType }}>
      {children}
    </BackendContext.Provider>
  )
}

function useBackendContext() {
  const ctx = React.useContext(BackendContext)
  if (ctx == null) throw new Error(`Backend not provided.`)
  return ctx
}

/** Exposes a property to get the current backend. */
export function useBackend() {
  const { backend } = useBackendContext()
  return { backend }
}

/** Exposes a property to set the current backend. */
export function useSetBackend() {
  const { setBackend, setBackendWithoutSavingType } = useBackendContext()
  return { setBackend, setBackendWithoutSavingType }
}
