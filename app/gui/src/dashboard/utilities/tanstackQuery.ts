/**
 * @file A collection of utility functions for Tanstack Query.
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import {
  useQueryClient,
  type DefaultError,
  type EnsureQueryDataOptions,
  type QueryClient,
  type QueryKey,
  type UseMutationOptions,
  type UseMutationResult,
} from '@tanstack/react-query'
import * as vueQuery from '@tanstack/vue-query'

/**
 * Return a function that will execute the given mutation.
 * Unlike `useMutation`, this function returns a function instead of the state of the mutation.
 */
export function useMutationCallback<
  TData = unknown,
  TError = DefaultError,
  TVariables = void,
  TContext = unknown,
>(
  options: UseMutationOptions<TData, TError, TVariables, TContext>,
  queryClient?: QueryClient,
): UseMutationResult<TData, TError, TVariables, TContext>['mutateAsync'] {
  const ctxQueryClient = useQueryClient(queryClient)

  return useEventCallback<UseMutationResult<TData, TError, TVariables, TContext>['mutateAsync']>(
    (variables) =>
      ctxQueryClient
        .getMutationCache()
        .build<TData, TError, TVariables, TContext>(ctxQueryClient, options)
        .execute(variables),
  )
}

/**
 * Return a function that will execute the given query.
 * Unlike `useQuery`, this function returns a function instead of the state of the query.
 */
export function useEnsureQueryData(queryClient?: QueryClient) {
  const ctxQueryClient = useQueryClient(queryClient)

  return useEventCallback(
    <
      TQueryFnData,
      TError = DefaultError,
      TData = TQueryFnData,
      TQueryKey extends QueryKey = QueryKey,
    >(
      options: EnsureQueryDataOptions<TQueryFnData, TError, TData, TQueryKey>,
    ) => ctxQueryClient.ensureQueryData(options),
  )
}

/**
 * Return a function that will execute the given query.
 * Unlike `useQuery`, this function returns a function instead of the state of the query.
 */
export function useVueEnsureQueryData() {
  const ctxQueryClient = vueQuery.useQueryClient()

  return <
    TQueryFnData,
    TError = DefaultError,
    TData = TQueryFnData,
    TQueryKey extends QueryKey = QueryKey,
  >(
    options: EnsureQueryDataOptions<TQueryFnData, TError, TData, TQueryKey>,
  ) => ctxQueryClient.ensureQueryData(options)
}
