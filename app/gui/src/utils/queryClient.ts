/** @file Tanstack Query client for Enso IDE and dashboard. */

import { useCallbackRegistry } from '$/utils/data/callbacks'
import { cloneDeepUnref } from '$/utils/data/reactive'
import * as queryCore from '@tanstack/query-core'
import type { AsyncStorage, StoragePersisterOptions } from '@tanstack/query-persist-client-core'
import { experimental_createQueryPersister as createPersister } from '@tanstack/query-persist-client-core'
import * as vueQuery from '@tanstack/vue-query'
import { toRaw } from 'vue'

declare module '@tanstack/query-core' {
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

// Internal tanstack type; we can use it type-safely though we lack API stability guarantees.
type QueryCacheConfig = Required<Required<ConstructorParameters<typeof queryCore.QueryCache>>[0]>
type OnQuerySettled = QueryCacheConfig['onSettled']
type OnQuerySuccess = QueryCacheConfig['onSuccess']
type OnQueryError = QueryCacheConfig['onError']

// Internal tanstack type; we can use it type-safely though we lack API stability guarantees.
type MutationCacheConfig = Required<
  Required<ConstructorParameters<typeof queryCore.MutationCache>>[0]
>
type OnMutate = MutationCacheConfig['onMutate']
type OnMutationSettled = MutationCacheConfig['onSettled']
type OnMutationSuccess = MutationCacheConfig['onSuccess']
type OnMutationError = MutationCacheConfig['onError']

interface QueryHooks {
  onSuccess: (callback: OnQuerySuccess) => void
  onSettled: (callback: OnQuerySettled) => void
  onError: (callback: OnQueryError) => void
}
function useQueryCache(): {
  queryCache: queryCore.QueryCache
  queryHooks: QueryHooks
} {
  const onSettled = useCallbackRegistry<Parameters<OnQuerySettled>>()
  const onSuccess = useCallbackRegistry<Parameters<OnQuerySuccess>>()
  const onError = useCallbackRegistry<Parameters<OnQueryError>>()
  const config = {
    onSettled: onSettled.run,
    onSuccess: onSuccess.run,
    onError: onError.run,
  }
  return {
    queryCache: new queryCore.QueryCache(config),
    queryHooks: {
      onSettled: onSettled.register,
      onSuccess: onSuccess.register,
      onError: onError.register,
    },
  }
}

interface MutationHooks {
  onMutate: (callback: OnMutate) => void
  onSuccess: (callback: OnMutationSuccess) => void
  onSettled: (callback: OnMutationSettled) => void
  onError: (callback: OnMutationError) => void
}
function useMutationCache(): {
  mutationCache: queryCore.MutationCache
  mutationHooks: MutationHooks
} {
  const onMutate = useCallbackRegistry<Parameters<OnMutate>>()
  const onSettled = useCallbackRegistry<Parameters<OnMutationSettled>>()
  const onSuccess = useCallbackRegistry<Parameters<OnMutationSuccess>>()
  const onError = useCallbackRegistry<Parameters<OnMutationError>>()
  const config = {
    onMutate: onMutate.run,
    onSettled: onSettled.run,
    onSuccess: onSuccess.run,
    onError: onError.run,
  }
  return {
    mutationCache: new queryCore.MutationCache(config),
    mutationHooks: {
      onMutate: onMutate.register,
      onSettled: onSettled.register,
      onSuccess: onSuccess.register,
      onError: onError.register,
    },
  }
}

declare const brandRaw: unique symbol
/**
 * A value that is known not to be a reactive proxy; this marker type can be used to ensure
 * comparisons are free of identity hazards.
 */
type RawValue<T> = T & { [brandRaw]: never }

/** Uniquely identifies a `Mutation`. */
type MutationKey = RawValue<queryCore.Mutation<unknown, unknown>>

function useInvalidation({
  mutationHooks,
  queryHooks,
  queryClient,
}: {
  mutationHooks: MutationHooks
  queryHooks: QueryHooks
  queryClient: QueryClient
}) {
  const mutationKey = toRaw as (mutation: queryCore.Mutation<unknown, unknown>) => MutationKey

  const invalidationKeys = new WeakMap<MutationKey, InvalidationKeys>()

  mutationHooks.onMutate(async (_variables, mutation) => {
    if (invalidationKeys.has(mutationKey(mutation))) {
      // A `Mutation` may begin execution again, for example, if it is re-attempted from an
      // `onError` callback. In this case, we still use the values of the invalidation keys as
      // of the time the mutation was first initiated, which is when any necessary state was
      // captured in its `variables`.
      return
    }
    const keys = evaluateInvalidationKeys(mutation)
    if (keys) invalidationKeys.set(mutationKey(mutation), keys)
  })
  mutationHooks.onSuccess((_data, _variables, _context, mutation) => {
    const keys = invalidationKeys.get(mutationKey(mutation))
    if (keys) return performInvalidations(queryClient, keys)
  })
  // In dev mode, run {@link cloneDeepUnref} to trigger its dev-mode checks for a query's queryKey.
  DEV: queryHooks.onSettled((_data, _error, { queryKey }) => void cloneDeepUnref(queryKey))
}

/** Create a new Tanstack Query client. */
export async function createQueryClient<TStorageValue = string>(
  options: QueryClientOptions<TStorageValue> = {},
): Promise<QueryClient> {
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

  const { queryCache, queryHooks } = useQueryCache()
  const { mutationCache, mutationHooks } = useMutationCache()

  const queryClient: QueryClient = new vueQuery.QueryClient({
    queryCache,
    mutationCache,
    defaultOptions: {
      queries: {
        ...(persister != null ? { persister: persister.persisterFn } : {}),
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
  useInvalidation({ mutationHooks, queryHooks, queryClient })
  await persister?.restoreQueries(queryClient)

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

interface InvalidationKeys {
  toAwait: queryCore.QueryKey[]
  toIgnore: queryCore.QueryKey[]
  refetchType: Required<queryCore.InvalidateQueryFilters>['refetchType']
}

function evaluateInvalidationKeys(
  mutation: queryCore.Mutation<unknown, unknown>,
): InvalidationKeys | undefined {
  if (!mutation.meta) return

  const metaAwaitInvalidates = mutation.meta.awaitInvalidates ?? false
  const metaInvalidates = cloneDeepUnref(mutation.meta.invalidates) ?? []
  const refetchType = mutation.meta?.refetchType ?? 'active'

  return (
    Array.isArray(metaAwaitInvalidates) ?
      { toAwait: cloneDeepUnref(metaAwaitInvalidates), toIgnore: metaInvalidates, refetchType }
    : metaAwaitInvalidates ? { toAwait: metaInvalidates, toIgnore: [], refetchType }
    : { toAwait: [], toIgnore: metaInvalidates, refetchType }
  )
}

async function performInvalidations(
  queryClient: QueryClient,
  toInvalidate: InvalidationKeys,
): Promise<void> {
  const { toAwait, toIgnore, refetchType } = toInvalidate
  const invalidateByKeys = (queryKeys: queryCore.QueryKey[]) =>
    queryKeys.map((queryKey) =>
      queryClient.invalidateQueries({
        predicate: (query) => queryCore.matchQuery({ queryKey }, query),
        refetchType,
      }),
    )
  void invalidateByKeys(toIgnore)
  await Promise.all(invalidateByKeys(toAwait))
}
