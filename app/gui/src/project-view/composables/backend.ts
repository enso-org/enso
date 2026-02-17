import { useBackends } from '$/providers/backends'
import {
  backendBaseOptions,
  backendQueryKey,
  INVALIDATE_ALL_QUERIES,
  INVALIDATION_MAP,
  type BackendMutationMethod,
  type BackendQueryMethod,
} from '$/utils/backendQuery'
import type { ToValue } from '@/util/reactivity'
import type {
  UseMutationOptions,
  UseMutationReturnType,
  UseQueryOptions,
} from '@tanstack/vue-query'
import { useMutation, useQuery, useQueryClient } from '@tanstack/vue-query'
import type { Backend } from 'enso-common/src/services/Backend'
import type { HttpClient } from 'enso-common/src/services/HttpClient'
import { computed, toValue, type UnwrapRef } from 'vue'
// eslint-disable-next-line vue/prefer-import-from-vue
import '@vue/reactivity'

declare module '@vue/reactivity' {
  interface RefUnwrapBailTypes {
    guiBailTypes: Backend | HttpClient
  }
}

type ExtraOptions = Omit<UseQueryOptions, 'queryKey' | 'queryFn' | 'enabled' | 'networkMode'>

const noPersist = { meta: { persist: false } }
const noFresh = { staleTime: 0 }
const methodDefaultOptions: Partial<Record<BackendQueryMethod, ExtraOptions>> = {
  listDirectory: { ...noPersist, ...noFresh },
  getFileDetails: { ...noPersist },
  getAssetDetails: { ...noPersist },
}

/** Commonly used options for tanstack queries to backend. */
export function backendQueryOptions<Method extends BackendQueryMethod, B extends Backend | null>(
  method: Method,
  args: ToValue<Parameters<Backend[Method]> | undefined>,
  backend: B,
) {
  return {
    ...backendBaseOptions(backend),
    ...(methodDefaultOptions[method] ?? {}),
    queryKey: computed(() => {
      const argsValue = toValue(args)
      return argsValue ? backendQueryKey(backend, method, argsValue) : []
    }),
    queryFn: (): Promise<
      B extends Backend ? Awaited<ReturnType<Backend[Method]>>
      : Awaited<ReturnType<Backend[Method]>> | null
    > =>
      backend ?
        (backend[method] as any).apply(backend, toValue(args)!)
      : (Promise.resolve(null) as any),
    enabled: computed(() => !!backend && !!toValue(args)),
  }
}

type MutationOptions<Method extends BackendMutationMethod, B extends Backend | null> = ToValue<
  Omit<
    UnwrapRef<
      UseMutationOptions<
        B extends Backend ? Awaited<ReturnType<Backend[Method]>>
        : Awaited<ReturnType<Backend[Method]>> | null,
        Error,
        Parameters<Backend[Method]>
      >
    >,
    'mutationFn'
  > & { invalidate?: boolean }
>

/**
 * Create Tanstack Query mutation options for given backend method call.
 */
export function backendMutationOptions<
  Method extends BackendMutationMethod,
  B extends Backend | null,
>(
  method: Method,
  backend: ToValue<B>,
  options?: MutationOptions<Method, B>,
): UseMutationOptions<
  B extends Backend ? Awaited<ReturnType<Backend[Method]>>
  : Awaited<ReturnType<Backend[Method]>> | null,
  Error,
  Parameters<Backend[Method]>
> {
  return computed(() => {
    const opts = toValue(options)
    const backendVal = toValue(backend)
    const invalidates =
      opts?.invalidate === false ?
        []
      : (INVALIDATION_MAP[method]?.map((queryMethod) =>
          queryMethod === INVALIDATE_ALL_QUERIES ?
            [backendVal?.type]
          : [backendVal?.type, queryMethod],
        ) ?? [])
    return {
      ...backendBaseOptions(backendVal),
      ...opts,
      mutationKey: [backendVal?.type, method, ...(toValue(opts?.mutationKey) ?? [])],
      mutationFn: (args) =>
        backendVal ? (backendVal[method] as any)(...args) : (Promise.resolve(null) as any),
      meta: {
        invalidates,
        awaitInvalidates: true,
        refetchType:
          invalidates.some((key) => key[1] === 'listDirectory') ?
            ('all' as const)
          : ('active' as const),
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
  const { localBackend: project, remoteBackend: remote } = useBackends()
  const backend: Backend | null = which === 'project' ? project : remote

  /** Perform the specified query, and keep the result up-to-date if the provided arguments change. */
  function query<Method extends BackendQueryMethod>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ) {
    return useQuery(backendQueryOptions(method, args, backend))
  }

  function fetch<Method extends BackendQueryMethod>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ) {
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
  ) {
    return queryClient.ensureQueryData(backendQueryOptions(method, args, backend))
  }

  function mutation<Method extends BackendMutationMethod>(
    method: Method,
    options?: MutationOptions<Method, Backend | null>,
  ): UseMutationReturnType<
    Awaited<ReturnType<Backend[Method]>> | null,
    Error,
    Parameters<Backend[Method]>,
    unknown
  > {
    return useMutation(backendMutationOptions<Method, Backend | null>(method, backend, options))
  }

  return { query, fetch, prefetch, ensureQueryData, mutation }
}

export type Mutation = ReturnType<typeof useBackend>['mutation']
