/** @file The React provider for keyboard and mouse localStorage, along with hooks to use the provider
 * via the shared React context. */
import * as React from 'react'

import LocalStorage from '#/utilities/LocalStorage'

// ===========================
// === LocalStorageContext ===
// ===========================

/** State contained in a `LocalStorageContext`. */
export interface LocalStorageContextType {
  readonly localStorage: LocalStorage
}

let globalLocalStorage: LocalStorage | null = null
const LocalStorageContext = React.createContext<LocalStorageContextType>({
  /** Return the global instance of {@link LocalStorage}. */
  get localStorage() {
    return (globalLocalStorage ??= new LocalStorage())
  },
})

/** Props for a {@link LocalStorageProvider}. */
export interface LocalStorageProviderProps extends React.PropsWithChildren {
  readonly localStorage?: LocalStorage
}

// ============================
// === LocalStorageProvider ===
// ============================

/** A React Provider that lets components get the shortcut registry. */
export default function LocalStorageProvider(props: LocalStorageProviderProps) {
  const { children } = props
  const localStorage = React.useMemo(() => new LocalStorage(), [])

  return (
    <LocalStorageContext.Provider value={{ localStorage }}>{children}</LocalStorageContext.Provider>
  )
}

/** Exposes a property to get the shortcut registry. */
export function useLocalStorage() {
  return React.useContext(LocalStorageContext)
}
