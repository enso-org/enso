import { onlineManager, type UseQueryReturnType } from '@tanstack/vue-query'

/**
 * Wait for data to be ready
 *
 * Basicly same as `query.suspense`, but in offline mode it just returns instead of trying to fetch
 * data before.
 */
export async function waitForData<TData, TError>(query: UseQueryReturnType<TData, TError>) {
  if (!onlineManager.isOnline() && query.data !== undefined) return query
  return query.suspense()
}
