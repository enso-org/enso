/** @file Utilities related to the `react-query` library. */
import {
  matchQuery,
  useQueryClient,
  type DefaultError,
  type DefinedInitialDataOptions,
  type Query,
  type QueryFilters,
  type QueryKey,
} from '@tanstack/react-query'
import { useSyncExternalStore } from 'react'

export const STATIC_QUERY_OPTIONS = {
  meta: { persist: false },
  staleTime: Infinity,
  gcTime: Infinity,
  refetchOnMount: false,
  refetchInterval: false,
  refetchOnReconnect: false,
  refetchIntervalInBackground: false,
} as const satisfies Partial<DefinedInitialDataOptions>

/** Reactively listen to a subset of filters, rather tha just one. */
export function useCachedQueries<
  TQueryFnData = unknown,
  TError = DefaultError,
  TData = TQueryFnData,
  TQueryKey extends QueryKey = QueryKey,
>(filters: QueryFilters) {
  const queryClient = useQueryClient()
  const queryCache = queryClient.getQueryCache()
  return useSyncExternalStore(
    (onChange) =>
      queryCache.subscribe((changes) => {
        if (changes.type !== 'added' && changes.type !== 'removed' && changes.type !== 'updated')
          return
        if (!matchQuery(filters, changes.query)) return
        onChange()
      }),
    () =>
      // eslint-disable-next-line no-restricted-syntax
      queryCache.findAll(filters) as unknown as readonly Query<
        TQueryFnData,
        TError,
        TData,
        TQueryKey
      >[],
  )
}
