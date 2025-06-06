import * as react from 'react'
import { LocationQueryValue } from 'vue-router'
import { QueryParams } from '../queryParams'
import { useInReactFunction, useVueValue } from './common'

export const QueryParamsContext = react.createContext<QueryParams | null>(null)
export const useQueryParams = useInReactFunction(QueryParamsContext)

export function useQueryParam(name: string) {
  const params = useQueryParams()
  const value = useVueValue(react.useCallback(() => params.get(name), [params, name]))
  const set = react.useCallback(
    (newVal: LocationQueryValue, replace?: boolean) => params.set(name, newVal, replace),
    [params, name],
  )
  const clear = react.useCallback(
    (replace?: boolean) => params.clear(name, replace),
    [params, name],
  )

  return [value, set, clear] as const
}
