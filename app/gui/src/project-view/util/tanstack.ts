import { onlineManager, type UseQueryReturnType } from '@tanstack/vue-query'

/**
 * Wait for data to be ready
 *
 * Basicly same as `query.promise`, but in offline mode it just returns instead of trying to fetch
 * data before.
 */
export async function waitForData<TData, TError>(query: UseQueryReturnType<TData, TError>) {
  if (!onlineManager.isOnline() && query.data.value !== undefined) return query
  return query.promise.value
}
