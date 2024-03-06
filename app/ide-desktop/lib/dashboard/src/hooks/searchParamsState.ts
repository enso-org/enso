/**
 * @file
 *
 * Search params state hook store a value in the URL search params.
 */

import { type SetStateAction } from 'react'

import { useSearchParams } from 'react-router-dom'

import { useEventCallback } from '#/hooks/eventCallback'

import { useLazyMemo } from './useLazyMemo'

/**
 *
 */
export type ParamKeyValuePair = [string, string]

/**
 *
 */
export function useSearchParamsState<T = any>(key: string, defaultValue: T | (() => T)) {
  const [searchParams, setSearchParams] = useSearchParams()

  const lazyDefaultValueInitializer = useLazyMemo(defaultValue, [defaultValue])

  const value = (() => {
    const maybeValue = searchParams.get(key)
    const _default = lazyDefaultValueInitializer()

    return maybeValue != null ? JSON.parse(maybeValue) : _default
  })()

  const setValue = useEventCallback((nextValue: SetStateAction<T>) => {
    if (nextValue instanceof Function) {
      nextValue = nextValue(value)
    }

    if (nextValue === value) {
      return
    }

    if (nextValue === lazyDefaultValueInitializer()) {
      clear()
      return
    }

    searchParams.set(key, JSON.stringify(nextValue))
    setSearchParams(searchParams)
  })

  const clear = useEventCallback(() => {
    searchParams.delete(key)
    setSearchParams(searchParams)
  })

  return [value, setValue, clear] as const
}
