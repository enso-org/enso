import { injectProjectBackend } from '@/providers/backend'
import type { ToValue } from '@/util/reactivity'
import type {
  UseMutationOptions,
  UseMutationReturnType,
  UseQueryOptions,
  UseQueryReturnType,
} from '@tanstack/vue-query'
import { useMutation, useQuery, useQueryClient } from '@tanstack/vue-query'
import type { BackendMutationMethod, BackendQueryMethod } from 'enso-common/src/backendQuery'
import {
  backendBaseOptions,
  backendQueryKey,
  INVALIDATE_ALL_QUERIES,
  INVALIDATION_MAP,
} from 'enso-common/src/backendQuery'
import Backend from 'enso-common/src/services/Backend'
import { computed, toValue, type UnwrapRef } from 'vue'

type ExtraOptions = Omit<UseQueryOptions, 'queryKey' | 'queryFn' | 'enabled' | 'networkMode'>

const noPersist = { meta: { persist: false } }
const noFresh = { staleTime: 0 }
const methodDefaultOptions: Partial<Record<BackendQueryMethod, ExtraOptions>> = {
  listDirectory: { ...noPersist, ...noFresh },
  getFileDetails: { ...noPersist },
}

function backendQueryOptions<Method extends BackendQueryMethod>(
  method: Method,
  args: ToValue<Parameters<Backend[Method]> | undefined>,
  backend: Backend | null,
) {
  return {
    ...backendBaseOptions(backend),
    ...(methodDefaultOptions[method] ?? {}),
    queryKey: computed(() => {
      const argsValue = toValue(args)
      return argsValue ? backendQueryKey(backend, method, argsValue) : []
    }),
    queryFn: () => backend && (backend[method] as any).apply(backend, toValue(args)!),
    enabled: computed(() => !!backend && !!toValue(args)),
  }
}

type MutationOptions<Method extends BackendMutationMethod> = ToValue<
  Omit<
    UnwrapRef<
      UseMutationOptions<
        Awaited<ReturnType<Backend[Method]>> | undefined,
        Error,
        Parameters<Backend[Method]>
      >
    >,
    'mutationFn' | 'mutationKey'
  > & { invalidate?: boolean }
>

function backendMutationOptions<Method extends BackendMutationMethod>(
  method: Method,
  backend: Backend | null,
  options?: MutationOptions<Method>,
): UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>> | undefined,
  Error,
  Parameters<Backend[Method]>
> {
  return computed(() => {
    const opts = toValue(options)
    const invalidates =
      opts?.invalidate === false ?
        []
      : (INVALIDATION_MAP[method]?.map((queryMethod) =>
          queryMethod === INVALIDATE_ALL_QUERIES ? [backend?.type] : [backend?.type, queryMethod],
        ) ?? [])
    return {
      ...backendBaseOptions(backend),
      mutationKey: [backend?.type, method],
      mutationFn: (args) => (backend ? (backend[method] as any)(...args) : undefined),
      ...opts,
      meta: {
        invalidates,
        awaitInvalidates: true,
        refetchType: invalidates.some((key) => key[1] === 'listDirectory') ? 'all' : 'active',
        ...opts?.meta,
      },
    }
  })
}

/**
 * Composable providing access to the backend API.
 * @param which - Whether to use the remote backend, or the current project's backend (which may be the remote backend,
 * or a local backend).
 */
export function useBackend(which: 'remote' | 'project') {
  const queryClient = useQueryClient()
  const { project, remote } = injectProjectBackend()
  const backend = which === 'project' ? project : remote

  /** Perform the specified query, and keep the result up-to-date if the provided arguments change. */
  function query<Method extends BackendQueryMethod>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ): UseQueryReturnType<Awaited<ReturnType<Backend[Method]>>, Error> {
    return useQuery(backendQueryOptions(method, args, backend))
  }

  function fetch<Method extends BackendQueryMethod>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ): Promise<Awaited<ReturnType<Backend[Method]>>> {
    return queryClient.fetchQuery(backendQueryOptions(method, args, backend))
  }

  /** Enable prefetching of the specified query. */
  function prefetch<Method extends BackendQueryMethod>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ) {
    return queryClient.prefetchQuery(backendQueryOptions(method, args, backend))
  }

  /** Return query results from the cache (even if stale), or if no cached data is available fetch the data. */
  function ensureQueryData<Method extends BackendQueryMethod>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ): Promise<Awaited<ReturnType<Backend[Method]>>> {
    return queryClient.ensureQueryData(backendQueryOptions(method, args, backend))
  }

  function mutation<Method extends BackendMutationMethod>(
    method: Method,
    options?: MutationOptions<Method>,
  ): UseMutationReturnType<
    Awaited<ReturnType<Backend[Method]>> | undefined,
    Error,
    Parameters<Backend[Method]>,
    unknown
  > {
    return useMutation(backendMutationOptions(method, backend, options))
  }

  return { query, fetch, prefetch, ensureQueryData, mutation }
}

export type Mutation = ReturnType<typeof useBackend>['mutation']
