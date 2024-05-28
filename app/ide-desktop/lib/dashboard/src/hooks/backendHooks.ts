/** @file Hooks for interacting with the backend. */
import * as reactQuery from '@tanstack/react-query'

import type Backend from '#/services/Backend'

// =======================
// === useBackendQuery ===
// =======================

/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends keyof Backend>(
  backend: Backend,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Omit<
    reactQuery.UseQueryOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      ReturnType<Extract<Backend[Method], (...args: never) => unknown>>,
      unknown[]
    >,
    'queryFn'
  >
) {
  return reactQuery.useQuery<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
    Error,
    ReturnType<Extract<Backend[Method], (...args: never) => unknown>>,
    unknown[]
  >({
    ...options,
    queryKey: ['backend', method, ...args, ...(options?.queryKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend[method] as any)(...args),
  })
}

// ==========================
// === useBackendMutation ===
// ==========================

/** Wrap a backend method call in a React Query Mutation. */
export function useBackendMutation<Method extends keyof Backend>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
      unknown
    >,
    'mutationFn'
  >
) {
  return reactQuery.useMutation<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
    Error,
    Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
    unknown
  >({
    ...options,
    mutationKey: ['backend', method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: args => (backend[method] as any)(...args),
  })
}
