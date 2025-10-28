/**
 * @file The React provider for localStorage, along with hooks to use the provider
 * via the shared React context.
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { LocalStorageData, LocalStorageKey } from '#/utilities/LocalStorage'
import { useLocalStorage } from '$/providers/react'
import * as React from 'react'

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
export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue?: LocalStorageData[K],
  options: LocalStorageStateOptions<K> = {},
): readonly [
  value: LocalStorageData[K] | undefined,
  setValue: (newValue: LocalStorageData[K] | undefined) => void,
] {
  const localStorage = useLocalStorage()
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
      privateSetValue((currentValue) => {
        const nextValue = typeof newValue === 'function' ? newValue(currentValue) : newValue
        if (nextValue === undefined) {
          localStorage.delete(key)
        } else {
          localStorage.set(key, nextValue)
        }

        return nextValue
      })
    },
  )

  React.useEffect(
    () =>
      localStorage.subscribe(key, (newValue) => {
        privateSetValue(newValue ?? defaultValue)
      }),
    [defaultValue, key, localStorage],
  )

  return [value, setValue]
}
