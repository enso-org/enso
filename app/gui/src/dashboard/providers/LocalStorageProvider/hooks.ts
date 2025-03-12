/** @file Hooks for `LocalStorageProvider`. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { LocalStorageData, LocalStorageKey } from '#/utilities/LocalStorage'
import { useContext, useEffect, useState, type SetStateAction } from 'react'
import invariant from 'tiny-invariant'
import { LocalStorageContext } from './constants'

/** Exposes a property to get the shortcut registry. */
export function useLocalStorage() {
  const context = useContext(LocalStorageContext)
  invariant(context, '`useLocalStorage` must be used within a `LocalStorageProvider`.')
  return context
}

export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
): readonly [
  value: LocalStorageData[K] | undefined,
  setValue: (newValue: SetStateAction<LocalStorageData[K] | undefined>) => void,
]

export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue: LocalStorageData[K],
): readonly [
  value: LocalStorageData[K],
  setValue: (newValue: SetStateAction<LocalStorageData[K]>) => void,
]

/** Subscribe to Local Storage updates for a specific key. */
export function useLocalStorageState<K extends LocalStorageKey>(
  key: K,
  defaultValue?: LocalStorageData[K],
): readonly [
  value: LocalStorageData[K] | undefined,
  setValue: (newValue: LocalStorageData[K] | undefined) => void,
] {
  const { localStorage } = useLocalStorage()

  const [value, privateSetValue] = useState<LocalStorageData[K] | undefined>(
    () => localStorage.get(key) ?? defaultValue,
  )

  const setValue = useEventCallback((newValue: SetStateAction<LocalStorageData[K] | undefined>) => {
    privateSetValue((currentValue) => {
      const nextValue = typeof newValue === 'function' ? newValue(currentValue) : newValue

      if (nextValue === undefined) {
        localStorage.delete(key)
      } else {
        localStorage.set(key, nextValue)
      }

      return nextValue
    })
  })

  useEffect(
    () =>
      localStorage.subscribe(key, (newValue) => {
        privateSetValue(newValue ?? defaultValue)
      }),
    [defaultValue, key, localStorage],
  )

  return [value, setValue]
}
