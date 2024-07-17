import type { ToValue } from '@/util/reactivity'
import type { QueryKey } from '@tanstack/vue-query'
import { computed, toValue } from 'vue'

export function useQueryOptions<Parameters, Data>(
  parameters: ToValue<Parameters | undefined>,
  queryKey: (parameters: Parameters) => QueryKey,
  queryFn: (parameters: Parameters) => Data,
) {
  return {
    queryKey: computed(() => {
      const paramsValue = toValue(parameters)
      return paramsValue ? queryKey(paramsValue) : []
    }),
    queryFn: () => queryFn(toValue(parameters)!),
    enabled: computed(() => !!toValue(parameters)),
  }
}
