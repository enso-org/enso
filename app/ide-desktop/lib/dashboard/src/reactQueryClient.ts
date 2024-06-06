/**
 * @file
 *
 * React Query client for the dashboard.
 */

import * as reactQuery from '@tanstack/react-query'

declare module '@tanstack/react-query' {
  /**
   * Specifies the invalidation behavior of a mutation.
   */
  interface Register {
    readonly mutationMeta: {
      /**
       * List of query keys to invalidate when the mutation succeeds.
       */
      readonly invalidates?: reactQuery.QueryKey[]
      /**
       * List of query keys to await invalidation before the mutation is considered successful.
       *
       * If `true`, all `invalidates` are awaited.
       *
       * If `false`, no invalidations are awaited.
       *
       * You can also provide an array of query keys to await.
       *
       * Queries that are not listed in invalidates will be ignored.
       * @default false
       */
      readonly awaitInvalidates?: reactQuery.QueryKey[] | boolean
    }
  }
}

// eslint-disable-next-line @typescript-eslint/no-magic-numbers
const DEFAULT_QUERY_STALE_TIME_MS = 2 * 60 * 1000

/**
 * Create a new React Query client.
 */
export function createReactQueryClient() {
  const queryClient: reactQuery.QueryClient = new reactQuery.QueryClient({
    mutationCache: new reactQuery.MutationCache({
      onSuccess: (_data, _variables, _context, mutation) => {
        const shouldAwaitInvalidates = mutation.meta?.awaitInvalidates ?? false
        const invalidates = mutation.meta?.invalidates ?? []
        const invalidatesToAwait = (() => {
          if (Array.isArray(shouldAwaitInvalidates)) {
            return shouldAwaitInvalidates
          } else {
            return shouldAwaitInvalidates ? invalidates : []
          }
        })()
        const invalidatesToIgnore = invalidates.filter(
          queryKey => !invalidatesToAwait.includes(queryKey)
        )

        for (const queryKey of invalidatesToIgnore) {
          void queryClient.invalidateQueries({
            predicate: query => reactQuery.matchQuery({ queryKey }, query),
          })
        }

        if (invalidatesToAwait.length > 0) {
          // eslint-disable-next-line no-restricted-syntax
          return Promise.all(
            invalidatesToAwait.map(queryKey =>
              queryClient.invalidateQueries({
                predicate: query => reactQuery.matchQuery({ queryKey }, query),
              })
            )
          )
        }
      },
    }),
    defaultOptions: {
      queries: {
        staleTime: DEFAULT_QUERY_STALE_TIME_MS,
        retry: (failureCount, error) => {
          // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          const statusesToIgnore = [401, 403, 404]
          const errorStatus =
            'status' in error && typeof error.status === 'number' ? error.status : -1

          if (statusesToIgnore.includes(errorStatus)) {
            return false
          } else {
            return failureCount < 3
          }
        },
      },
    },
  })

  return queryClient
}
