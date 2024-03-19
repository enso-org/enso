/**
 * @file
 *
 * Search params state hook store a value in the URL search params.
 */
import * as React from 'react'

import * as reactRouterDom from 'react-router-dom'

import * as eventCallback from '#/hooks/eventCallback'
import * as lazyMemo from '#/hooks/useLazyMemo'

import * as safeJsonParse from '#/utilities/safeJsonParse'

/**
 * The return type of the `useSearchParamsState` hook.
 */
type SearchParamsStateReturnType<T> = Readonly<
  [value: T, setValue: (nextValue: React.SetStateAction<T>) => void, clear: () => void]
>

/**
 * Hook that synchronize a state in the URL search params. It returns the value, a setter and a clear function.
 * @param key - The key to store the value in the URL search params.
 * @param defaultValue - The default value to use if the key is not present in the URL search params.
 * @param predicate - A function to check if the value is of the right type.
 */
export function useSearchParamsState<T = unknown>(
  key: string,
  defaultValue: T | (() => T),
  predicate: (unknown: unknown) => unknown is T = (unknown): unknown is T => true
): SearchParamsStateReturnType<T> {
  const [searchParams, setSearchParams] = reactRouterDom.useSearchParams()

  const lazyDefaultValueInitializer = lazyMemo.useLazyMemo(defaultValue, [])
  const predicateEventCallback = eventCallback.useEventCallback(predicate)

  const clear = eventCallback.useEventCallback((replace: boolean = false) => {
    searchParams.delete(key)
    setSearchParams(searchParams, { replace })
  })

  const rawValue = React.useMemo<T>(() => {
    const maybeValue = searchParams.get(key)
    const defaultValueFrom = lazyDefaultValueInitializer()

    return maybeValue != null
      ? safeJsonParse.safeJsonParse(maybeValue, defaultValueFrom, (unknown): unknown is T => true)
      : defaultValueFrom
  }, [key, lazyDefaultValueInitializer, searchParams])

  const isValueValid = predicateEventCallback(rawValue)

  const value = isValueValid ? rawValue : lazyDefaultValueInitializer()

  if (!isValueValid) {
    clear(true)
  }

  /**
   * Set the value in the URL search params. If the next value is the same as the default value, it will remove the key from the URL search params.
   * Function reference is always the same.
   * @param nextValue - The next value to set.
   * @returns void
   */
  const setValue = eventCallback.useEventCallback((nextValue: React.SetStateAction<T>) => {
    if (nextValue instanceof Function) {
      nextValue = nextValue(value)
    }

    if (nextValue === lazyDefaultValueInitializer()) {
      clear()
    } else {
      searchParams.set(key, JSON.stringify(nextValue))
      setSearchParams(searchParams)
    }
  })

  return [value, setValue, clear]
}
