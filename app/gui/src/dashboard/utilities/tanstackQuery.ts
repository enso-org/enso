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
  const ctxQueryClient = useQueryClient()
  const queryClientContext = queryClient ?? ctxQueryClient

  return useEventCallback<UseMutationResult<TData, TError, TVariables, TContext>['mutateAsync']>(
    (...args) =>
      queryClientContext
        .getMutationCache()
        .build<TData, TError, TVariables, TContext>(queryClientContext, options)
        // This is safe, because the function is typed outside of the callback.
        // eslint-disable-next-line no-restricted-syntax
        .execute(args as TVariables),
  )
}

/**
 * Return a function that will execute the given query.
 * Unlike `useQuery`, this function returns a function instead of the state of the query.
 */
export function useEnsureQueryData<
  TQueryFnData,
  TError = DefaultError,
  TData = TQueryFnData,
  TQueryKey extends QueryKey = QueryKey,
>(queryClient?: QueryClient) {
  const ctxQueryClient = useQueryClient()
  const queryClientContext = queryClient ?? ctxQueryClient

  return useEventCallback<
    (options: EnsureQueryDataOptions<TQueryFnData, TError, TData, TQueryKey>) => Promise<TData>
  >((options) => queryClientContext.ensureQueryData(options))
}
