/**
 * @file
 *
 * Search params state hook store a value in the URL search params.
 */
import * as React from 'react'

import * as appUtils from '$/appUtils'

import * as eventCallback from '#/hooks/eventCallbackHooks'
import * as lazyMemo from '#/hooks/useLazyMemoHooks'

import * as safeJsonParse from '#/utilities/safeJsonParse'
import { useQueryParam } from '$/providers/react/queryParams'

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
  const prefixedKey = `${appUtils.SEARCH_PARAMS_PREFIX}${key}`
  const [param, setParam, clearParam] = useQueryParam(prefixedKey)

  const lazyDefaultValueInitializer = lazyMemo.useLazyMemoHooks(defaultValue, [])

  const rawValue = (() => {
    const maybeValue = param
    const defaultValueFrom = lazyDefaultValueInitializer()

    return maybeValue != null ?
        safeJsonParse.safeJsonParse(maybeValue, defaultValueFrom, (unknown): unknown is T => true)
      : defaultValueFrom
  })()

  const isValueValid = predicate(rawValue)

  const value = isValueValid ? rawValue : lazyDefaultValueInitializer()

  React.useEffect(() => {
    if (!isValueValid) {
      clearParam(true)
    }
  }, [isValueValid, clearParam])
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
        clearParam(replace)
      } else {
        setParam(JSON.stringify(nextValue), replace)
      }
    },
  )

  return [value, setValue, clearParam]
}
