/**
 * @file
 *
 * Search params state hook store a value in the URL search params.
 */

import type * as React from 'react'

import * as reactRouterDom from 'react-router-dom'

import * as eventCallback from '#/hooks/eventCallback'

import * as safeJsonParse from '#/utilities/safeJsonParse'

import * as lazyMemo from './useLazyMemo'

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
 */
export function useSearchParamsState<T = unknown>(
  key: string,
  defaultValue: T | (() => T)
): SearchParamsStateReturnType<T> {
  const [searchParams, setSearchParams] = reactRouterDom.useSearchParams()

  const lazyDefaultValueInitializer = lazyMemo.useLazyMemo(defaultValue, [defaultValue])

  const value: T = (() => {
    const maybeValue = searchParams.get(key)
    const defaultValueFrom = lazyDefaultValueInitializer()

    return maybeValue != null
      ? safeJsonParse.safeJsonParse(maybeValue, defaultValueFrom, (unknown): unknown is T => true)
      : defaultValueFrom
  })()

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

  const clear = eventCallback.useEventCallback(() => {
    searchParams.delete(key)
    setSearchParams(searchParams)
  })

  return [value, setValue, clear]
}
