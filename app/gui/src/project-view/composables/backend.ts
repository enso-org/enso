import { injectBackend } from '@/providers/backend'
import type { ToValue } from '@/util/reactivity'
import {
  useQuery,
  useQueryClient,
  type UseQueryOptions,
  type UseQueryReturnType,
} from '@tanstack/vue-query'
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
  backend: Backend,
) {
  return {
    ...backendBaseOptions(backend),
    ...(methodDefaultOptions[method] ?? {}),
    queryKey: computed(() => {
      const argsValue = toValue(args)
      return argsValue ? backendQueryKey(backend, method, argsValue) : []
    }),
    queryFn: () => (backend[method] as any).apply(backend, toValue(args)!),
    enabled: computed(() => !!toValue(args)),
  }
}

export function useBackendQuery<Method extends BackendMethods>(
  method: Method,
  args: ToValue<Parameters<Backend[Method]> | undefined>,
): UseQueryReturnType<Awaited<ReturnType<Backend[Method]>>, Error> {
  const { backend } = injectBackend()
  return useQuery(backendQueryOptions(method, args, backend))
}

export function useBackendQueryPrefetching() {
  const queryClient = useQueryClient()
  const { backend } = injectBackend()

  function prefetch<Method extends BackendMethods>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ) {
    return queryClient.prefetchQuery(backendQueryOptions(method, args, backend))
  }
  function ensureQueryData<Method extends BackendMethods>(
    method: Method,
    args: ToValue<Parameters<Backend[Method]> | undefined>,
  ): Promise<Awaited<ReturnType<Backend[Method]>>> {
    return queryClient.ensureQueryData(backendQueryOptions(method, args, backend))
  }
  return { prefetch, ensureQueryData }
}
