/**
 * @file
 *
 * Tanstack Query client for Enso IDE and dashboard.
 */

import * as queryCore from '@tanstack/query-core'
import type { AsyncStorage, StoragePersisterOptions } from '@tanstack/query-persist-client-core'
import { experimental_createPersister as createPersister } from '@tanstack/query-persist-client-core'
import * as vueQuery from '@tanstack/vue-query'

declare module '@tanstack/query-core' {
  const DUMMY_MUTATION_POOL_SYMBOL: unique symbol
  /**
   * An enumeration of all mutation pool ids. Declaration merge into this interface to add a new
   * mutation pool id:
   *
   * ```ts
   * declare module '@tanstack/query-core' {
   *   interface MutationPools {
   *     myNewPoolId: true
   *   }
   * }
   * ```
   */
  export interface MutationPools {
    readonly [DUMMY_MUTATION_POOL_SYMBOL]: true
  }

  export type MutationPoolId = keyof MutationPools

  /** Query client with additional methods. */
  interface QueryClient {
    /**
     * Clear the cache stored in Tanstack Query and the persister storage.
     * Please use this method with caution, as it will clear all cache data.
     * Usually you should use `queryClient.invalidateQueries` instead.
     */
    readonly clearWithPersister: () => Promise<void>
    /** Clear the cache stored in the persister storage. */
    readonly nukePersister: () => Promise<void>
  }
  /** Specifies the invalidation behavior of a mutation. */
  interface Register {
    readonly mutationMeta: {
      /** List of query keys to invalidate when the mutation succeeds. */
      readonly invalidates?: queryCore.QueryKey[]
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
      readonly awaitInvalidates?: queryCore.QueryKey[] | boolean
      readonly refetchType?: queryCore.InvalidateQueryFilters['refetchType']
      readonly pool?: {
        id: MutationPoolId
        parallelism: number
      }
    }

    readonly queryMeta: {
      /**
       * Whether to persist the query cache in the storage. Defaults to `true`.
       * Use `false` to disable persistence for a specific query, for example for
       * a sensitive data or data that can't be persisted, e.g. class instances.
       * @default true
       */
      readonly persist?: boolean
    }
  }
}

/** Query Client type suitable for shared use in React and Vue. */
export type QueryClient = vueQuery.QueryClient

const DEFAULT_QUERY_STALE_TIME_MS = 0
const DEFAULT_QUERY_PERSIST_TIME_MS = 30 * 24 * 60 * 60 * 1000 // 30 days

const DEFAULT_BUSTER = 'v1.2'

export interface QueryClientOptions<TStorageValue = string> {
  readonly persisterStorage?: AsyncStorage<TStorageValue> & {
    readonly clear: () => Promise<void>
    readonly serialize?: StoragePersisterOptions<TStorageValue>['serialize']
    readonly deserialize?: StoragePersisterOptions<TStorageValue>['deserialize']
  }
}

/** Create a new Tanstack Query client. */
export function createQueryClient<TStorageValue = string>(
  options: QueryClientOptions<TStorageValue> = {},
): QueryClient {
  const { persisterStorage } = options

  queryCore.onlineManager.setOnline(navigator.onLine)

  let persister: ReturnType<typeof createPersister<TStorageValue>> | null = null
  if (persisterStorage) {
    persister = createPersister<TStorageValue>({
      storage: persisterStorage,
      // Prefer online first and don't rely on the local cache if user is online
      // fallback to the local cache only if the user is offline
      maxAge: DEFAULT_QUERY_PERSIST_TIME_MS,
      buster: DEFAULT_BUSTER,
      filters: { predicate: (query) => query.meta?.persist ?? true },
      prefix: 'enso:query-persist:',
      ...(persisterStorage.serialize != null ? { serialize: persisterStorage.serialize } : {}),
      ...(persisterStorage.deserialize != null ?
        { deserialize: persisterStorage.deserialize }
      : {}),
    })
  }

  const pools: Partial<
    Record<
      queryCore.MutationPoolId,
      { usedLanes: number; promise: Promise<void>; resolve: () => void; reject: () => void }
    >
  > = {}

  function promiseWithResolvers<T>() {
    let resolve!: (value: T | PromiseLike<T>) => void
    let reject!: (reason?: unknown) => void
    const promise = new Promise<T>((innerResolve, innerReject) => {
      resolve = innerResolve
      reject = innerReject
    })
    return { promise, resolve, reject }
  }

  const queryClient: QueryClient = new vueQuery.QueryClient({
    mutationCache: new queryCore.MutationCache({
      onMutate: async (_variables, mutation) => {
        const poolMeta = mutation.meta?.pool
        if (!poolMeta) {
          return
        }
        while ((pools[poolMeta.id]?.usedLanes ?? 0) >= poolMeta.parallelism) {
          await pools[poolMeta.id]?.promise
        }
        const poolInfo = (pools[poolMeta.id] ??= { usedLanes: 0, ...promiseWithResolvers() })
        poolInfo.usedLanes += 1
      },
      onSettled: (_data, error, _variables, _context, mutation) => {
        const poolMeta = mutation.meta?.pool
        if (poolMeta) {
          const poolInfo = (pools[poolMeta.id] ??= { usedLanes: 0, ...promiseWithResolvers() })
          poolInfo.usedLanes -= 1
          if (error != null) {
            poolInfo.reject()
          } else {
            poolInfo.resolve()
          }
          Object.assign(poolInfo, promiseWithResolvers())
        }
      },
      onSuccess: (_data, _variables, _context, mutation) => {
        const shouldAwaitInvalidates = mutation.meta?.awaitInvalidates ?? false
        const refetchType = mutation.meta?.refetchType ?? 'active'
        const invalidates = mutation.meta?.invalidates ?? []

        const invalidatesToAwait = (() => {
          if (Array.isArray(shouldAwaitInvalidates)) {
            return shouldAwaitInvalidates
          } else {
            return shouldAwaitInvalidates ? invalidates : []
          }
        })()

        const invalidatesToIgnore = invalidates.filter(
          (queryKey) => !invalidatesToAwait.includes(queryKey),
        )

        for (const queryKey of invalidatesToIgnore) {
          void queryClient.invalidateQueries({
            predicate: (query) => queryCore.matchQuery({ queryKey }, query),
            refetchType,
          })
        }

        if (invalidatesToAwait.length > 0) {
          return Promise.all(
            invalidatesToAwait.map((queryKey) =>
              queryClient.invalidateQueries({
                predicate: (query) => queryCore.matchQuery({ queryKey }, query),
                refetchType,
              }),
            ),
          )
        }
      },
    }),
    defaultOptions: {
      queries: {
        ...(persister != null ? { persister } : {}),
        // Default set to 'always' to don't pause ongoing queries
        // and make them fail.
        networkMode: 'always',
        refetchOnReconnect: 'always',
        staleTime: DEFAULT_QUERY_STALE_TIME_MS,
        // This allows to prefetch queries in the render phase. Enables returning
        // a promise from the `useQuery` hook, which is useful for the `Await` component,
        // which needs to prefetch the query in the render phase to be able to display
        // the error boundary/suspense fallback.
        // @see [experimental_prefetchInRender](https://tanstack.com/query/latest/docs/framework/react/guides/suspense#using-usequerypromise-and-reactuse-experimental)
        // eslint-disable-next-line camelcase
        experimental_prefetchInRender: true,
        retry: (failureCount, error: unknown) => {
          const statusesToIgnore = [403, 404]
          const errorStatus =
            (
              typeof error === 'object' &&
              error != null &&
              'status' in error &&
              typeof error.status === 'number'
            ) ?
              error.status
            : -1

          if (errorStatus === 401) {
            return true
          }

          if (statusesToIgnore.includes(errorStatus)) {
            return false
          }

          return failureCount < 3
        },
      },
    },
  })

  Object.defineProperty(queryClient, 'nukePersister', {
    value: () => persisterStorage?.clear(),
    enumerable: false,
    configurable: false,
    writable: false,
  })

  Object.defineProperty(queryClient, 'clearWithPersister', {
    value: () => {
      queryClient.clear()
      return queryClient.nukePersister()
    },
    enumerable: false,
    configurable: false,
    writable: false,
  })

  return queryClient
}
