/**
 * @file The React provider for localStorage, along with hooks to use the provider
 * via the shared React context.
 */
import * as React from 'react'

import { useEventCallback } from '#/hooks/eventCallbackHooks'

import LocalStorage, { type LocalStorageData, type LocalStorageKey } from '#/utilities/LocalStorage'

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
export type LocalStorageProviderProps = Readonly<React.PropsWithChildren>

// ============================
// === LocalStorageProvider ===
// ============================

/** A React Provider that lets components get the shortcut registry. */
export default function LocalStorageProvider(props: LocalStorageProviderProps) {
  const { children } = props
  const [localStorage] = React.useState(() => new LocalStorage())

  return (
    <LocalStorageContext.Provider value={{ localStorage }}>{children}</LocalStorageContext.Provider>
  )
}

/** Exposes a property to get the shortcut registry. */
export function useLocalStorage() {
  return React.useContext(LocalStorageContext)
}

export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue?: undefined,
): [
  value: LocalStorageData[K] | undefined,
  setValue: (newValue: LocalStorageData[K] | undefined) => void,
]

export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue: LocalStorageData[K],
): [value: LocalStorageData[K], setValue: (newValue: LocalStorageData[K] | undefined) => void]

/** Subscribe to Local Storage updates for a specific key. */
export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue?: LocalStorageData[K],
): [
  value: LocalStorageData[K] | undefined,
  setValue: (newValue: LocalStorageData[K] | undefined) => void,
] {
  const { localStorage } = useLocalStorage()

  const [value, privateSetValue] = React.useState<LocalStorageData[K] | undefined>(
    () => localStorage.get(key) ?? defaultValue,
  )

  const setValue = useEventCallback((newValue: LocalStorageData[K] | undefined) => {
    if (newValue === undefined) {
      localStorage.delete(key)
      privateSetValue(defaultValue)
    } else {
      localStorage.set(key, newValue)
      privateSetValue(newValue)
    }
  })

  return [value, setValue]
}
