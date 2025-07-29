import { onlineManager, UseQueryReturnType } from '@tanstack/vue-query'

export async function waitForData<TData, TError>(query: UseQueryReturnType<TData, TError>) {
  if (!onlineManager.isOnline() && query.data !== undefined) return query
  return query.suspense()
}
