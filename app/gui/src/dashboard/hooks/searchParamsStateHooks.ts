/**
 * @file
 *
 * Search params state hook store a value in the URL search params.
 */
import * as React from 'react'

import * as appUtils from '#/appUtils'

import * as eventCallback from '#/hooks/eventCallbackHooks'
import * as lazyMemo from '#/hooks/useLazyMemoHooks'

import * as safeJsonParse from '#/utilities/safeJsonParse'
import { useRouter } from '$/providers/react'
import { useCallback } from 'react'
import { type RouteLocationOptions } from 'vue-router'

/** The return type of the `useSearchParamsState` hook. */
type SearchParamsStateReturnType<T> = Readonly<
  [
    value: T,
    setValue: (nextValue: React.SetStateAction<T>, params?: SearchParamsSetOptions) => void,
    clear: (replace?: boolean) => void,
  ]
>

/** Set options for the `set` function. */
export interface SearchParamsSetOptions {
  readonly replace?: boolean
}

/**
 * Hook to synchronize a state in the URL search params. It returns the value, a setter and a clear function.
 * @param key - The key to store the value in the URL search params.
 * @param defaultValue - The default value to use if the key is not present in the URL search params.
 * @param predicate - A function to check if the value is of the right type.
 */
export function useSearchParamsState<T = unknown>(
  key: string,
  defaultValue: T | (() => T),
  predicate: (unknown: unknown) => unknown is T = (unknown): unknown is T => true,
): SearchParamsStateReturnType<T> {
  const { router, searchParams } = useRouter()

  const setSearchParams = useCallback(
    (
      nextSearchParams:
        | URLSearchParams
        | ((currentSearchParams: URLSearchParams) => URLSearchParams),
      options: RouteLocationOptions,
    ) => {
      const params = searchParams

      if (nextSearchParams instanceof Function) {
        nextSearchParams = nextSearchParams(params)
      }

      const query = Object.fromEntries(nextSearchParams.entries())
      void router.push({ query, ...options })
    },
    [router, searchParams],
  )

  const prefixedKey = `${appUtils.SEARCH_PARAMS_PREFIX}${key}`

  const lazyDefaultValueInitializer = lazyMemo.useLazyMemoHooks(defaultValue, [])

  const clear = eventCallback.useEventCallback((replace: boolean = false) => {
    setSearchParams(
      (currentSearchParams) => {
        currentSearchParams.delete(prefixedKey)
        return currentSearchParams
      },
      { replace },
    )
  })

  const rawValue = (() => {
    const maybeValue = searchParams.get(prefixedKey)
    const defaultValueFrom = lazyDefaultValueInitializer()

    return maybeValue != null ?
        safeJsonParse.safeJsonParse(maybeValue, defaultValueFrom, (unknown): unknown is T => true)
      : defaultValueFrom
  })()

  const isValueValid = predicate(rawValue)

  const value = isValueValid ? rawValue : lazyDefaultValueInitializer()

  React.useEffect(() => {
    if (!isValueValid) {
      clear(true)
    }
  }, [isValueValid, clear])
  /**
   * Set the value in the URL search params. If the next value is the same as the default value, it will remove the key from the URL search params.
   * Function reference is always the same.
   * @param nextValue - The next value to set.
   * @returns void
   */
  const setValue = eventCallback.useEventCallback(
    (nextValue: React.SetStateAction<T>, params: SearchParamsSetOptions = {}) => {
      const { replace = false } = params

      if (nextValue instanceof Function) {
        nextValue = nextValue(value)
      }

      if (nextValue === lazyDefaultValueInitializer()) {
        clear()
      } else {
        setSearchParams(
          (currentSearchParams) => {
            currentSearchParams.set(prefixedKey, JSON.stringify(nextValue))
            return currentSearchParams
          },
          { replace },
        )
      }
    },
  )

  return [value, setValue, clear]
}
