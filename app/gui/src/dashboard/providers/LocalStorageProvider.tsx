/**
 * @file The React provider for localStorage, along with hooks to use the provider
 * via the shared React context.
 */
import * as React from 'react'

import { useEventCallback } from '#/hooks/eventCallbackHooks'

import LocalStorage, { type LocalStorageData, type LocalStorageKey } from '#/utilities/LocalStorage'
import { use } from 'react'

/** State contained in a `LocalStorageContext`. */
export interface LocalStorageContextType {
  readonly localStorage: LocalStorage
}

const LocalStorageContext = React.createContext<LocalStorageContextType>({
  localStorage: LocalStorage.getInstance(),
})

/** Props for a {@link LocalStorageProvider}. */
export type LocalStorageProviderProps = Readonly<React.PropsWithChildren> & {
  readonly localStorage?: LocalStorage | undefined
}

/** A React Provider that lets components get the shortcut registry. */
export default function LocalStorageProvider(props: LocalStorageProviderProps) {
  const { children, localStorage } = props

  const finalLocalStorage = localStorage ?? use(LocalStorageContext).localStorage

  return (
    <LocalStorageContext.Provider value={{ localStorage: finalLocalStorage }}>
      {children}
    </LocalStorageContext.Provider>
  )
}

/** Exposes a property to get the shortcut registry. */
// eslint-disable-next-line react-refresh/only-export-components
export function useLocalStorage() {
  return React.useContext(LocalStorageContext)
}

/** Options for {@link useLocalStorageState}. */
export interface LocalStorageStateOptions<K extends LocalStorageKey> {
  readonly sanitize?: (value: LocalStorageData[K]) => LocalStorageData[K] | undefined
}

export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue?: undefined,
  options?: LocalStorageStateOptions<K>,
): readonly [
  value: LocalStorageData[K] | undefined,
  setValue: (newValue: React.SetStateAction<LocalStorageData[K] | undefined>) => void,
]

export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue: LocalStorageData[K],
  options?: LocalStorageStateOptions<K>,
): readonly [
  value: LocalStorageData[K],
  setValue: (newValue: React.SetStateAction<LocalStorageData[K]>) => void,
]

/** Subscribe to Local Storage updates for a specific key. */
// eslint-disable-next-line react-refresh/only-export-components
export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue?: LocalStorageData[K],
  options: LocalStorageStateOptions<K> = {},
): readonly [
  value: LocalStorageData[K] | undefined,
  setValue: (newValue: LocalStorageData[K] | undefined) => void,
] {
  const { localStorage } = useLocalStorage()
  const { sanitize } = options

  const [value, privateSetValue] = React.useState<LocalStorageData[K] | undefined>(() => {
    let savedValue: LocalStorageData[K] | undefined = localStorage.get(key)

    if (savedValue !== undefined && sanitize) {
      savedValue = sanitize(savedValue)
    }

    if (savedValue === undefined) {
      return defaultValue
    }

    return savedValue
  })

  const setValue = useEventCallback(
    (newValue: React.SetStateAction<LocalStorageData[K] | undefined>) => {
      let nextValue: LocalStorageData[K] | undefined = value

      privateSetValue((currentValue) => {
        nextValue = typeof newValue === 'function' ? newValue(currentValue) : newValue
        return nextValue
      })

      // We strictly must update the localStorage value here, because the
      // subscription will trigger (because it triggers since the subscription
      // is created in the `useEffect` below) an update with the old value in
      // state. And this potentially could cause the unintended side effect.
      if (nextValue === undefined) {
        localStorage.delete(key)
      } else {
        localStorage.set(key, nextValue)
      }
    },
  )

  const updateValueOnLocalStorageChange = useEventCallback(
    (newValue: LocalStorageData[K] | undefined) => {
      const nextValue = newValue ?? defaultValue

      privateSetValue((currentValue) => {
        if (currentValue === nextValue) {
          return currentValue
        }

        return nextValue
      })
    },
  )

  React.useEffect(
    () => localStorage.subscribe(key, updateValueOnLocalStorageChange),
    [key, localStorage, updateValueOnLocalStorageChange],
  )

  return [value, setValue]
}
