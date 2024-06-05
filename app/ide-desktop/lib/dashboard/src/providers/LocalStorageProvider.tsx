/** @file The React provider for localStorage, along with hooks to use the provider
 * via the shared React context. */
import * as React from 'react'

import * as refreshHooks from '#/hooks/refreshHooks'

import type * as localStorageModule from '#/utilities/LocalStorage'
import LocalStorage from '#/utilities/LocalStorage'

// ===========================
// === LocalStorageContext ===
// ===========================

/** State contained in a `LocalStorageContext`. */
export interface LocalStorageContextType {
  readonly localStorage: LocalStorage
}

// @ts-expect-error The default value will never be exposed, as using this without a `Provider`
// is a mistake.
const LocalStorageContext = React.createContext<LocalStorageContextType>(null)

/** Props for a {@link LocalStorageProvider}. */
export interface LocalStorageProviderProps extends Readonly<React.PropsWithChildren> {}

// ============================
// === LocalStorageProvider ===
// ============================

/** A React Provider that lets components get the shortcut registry. */
export default function LocalStorageProvider(props: LocalStorageProviderProps) {
  const { children } = props
  const [, doRefresh] = refreshHooks.useRefresh()

  const localStorage = React.useMemo(() => new LocalStorage(doRefresh), [doRefresh])

  return (
    <LocalStorageContext.Provider value={{ localStorage }}>{children}</LocalStorageContext.Provider>
  )
}

// =======================
// === useLocalStorage ===
// =======================

/** Exposes a property to get the shortcut registry. */
export function useLocalStorage() {
  return React.useContext(LocalStorageContext)
}

// ============================
// === useLocalStorageValue ===
// ============================

/** Update a {@link LocalStorage} value in `localStorage` when the returned `setValue` function
 * is called. */
export function useLocalStorageValue<Key extends localStorageModule.LocalStorageKey>(
  key: Key,
  getDefault: () => localStorageModule.LocalStorageData[Key]
): readonly [
  value: localStorageModule.LocalStorageData[Key],
  setValue: React.Dispatch<React.SetStateAction<localStorageModule.LocalStorageData[Key]>>,
] {
  const { localStorage } = useLocalStorage()
  const [value, setValueRaw] = React.useState(() => localStorage.get(key) ?? getDefault())

  const setValue = React.useCallback(
    (valueOrUpdater: React.SetStateAction<localStorageModule.LocalStorageData[Key]>) => {
      if (typeof valueOrUpdater !== 'function') {
        localStorage.set(key, valueOrUpdater)
        setValueRaw(valueOrUpdater)
      } else {
        setValueRaw(currentValue => {
          const newValue = valueOrUpdater(currentValue)
          localStorage.set(key, newValue)
          return newValue
        })
      }
    },
    [key, localStorage]
  )

  return [value, setValue]
}
