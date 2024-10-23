import { injectBackend } from '@/providers/backend'
import type { ToValue } from '@/util/reactivity'
import { useQuery, useQueryClient, UseQueryOptions, UseQueryReturnType } from '@tanstack/vue-query'
import type { BackendMethods } from 'enso-common/src/backendQuery'
import { backendBaseOptions, backendQueryKey } from 'enso-common/src/backendQuery'
import Backend from 'enso-common/src/services/Backend'
import { computed, toValue } from 'vue'

type ExtraOptions = Omit<UseQueryOptions, 'queryKey' | 'queryFn' | 'enabled' | 'networkMode'>

const noPersist = { meta: { persist: false } }
const noFresh = { staleTime: 0 }
const methodDefaultOptions: Partial<Record<BackendMethods, ExtraOptions>> = {
  listDirectory: { ...noPersist, ...noFresh },
  getFileDetails: { ...noPersist },
}

function backendQueryOptions<Method extends BackendMethods>(
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

/**
 * Composable providing access to the backend API.
 * @param which - Whether to use the remote backend, or the current project's backend (which may be the remote backend,
 * or a local backend).
 */
export function useBackend(which: 'remote' | 'project') {
  const queryClient = useQueryClient()
  const { project, remote } = injectBackend()
  const backend = which === 'project' ? project : remote

  /** Perform the specified query, and keep the result up-to-date if the provided arguments change. */
  function query<Method extends BackendMethods>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ): UseQueryReturnType<Awaited<ReturnType<Backend[Method]>>, Error> {
    return useQuery(backendQueryOptions(method, args, backend))
  }

  /** Enable prefetching of the specified query. */
  function prefetch<Method extends BackendMethods>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ) {
    return queryClient.prefetchQuery(backendQueryOptions(method, args, backend))
  }

  /** Return query results from the cache (even if stale), or if no cached data is available fetch the data. */
  function ensureQueryData<Method extends BackendMethods>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ): Promise<Awaited<ReturnType<Backend[Method]>>> {
    return queryClient.ensureQueryData(backendQueryOptions(method, args, backend))
  }

  return { query, prefetch, ensureQueryData }
}
