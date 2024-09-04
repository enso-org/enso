import type { DefinedInitialDataOptions } from '@tanstack/react-query'

export const STATIC_QUERY_OPTIONS = {
  meta: { persist: false },
  staleTime: Infinity,
  gcTime: Infinity,
  refetchOnMount: false,
  refetchInterval: false,
  refetchOnReconnect: false,
  refetchIntervalInBackground: false,
} as const satisfies Partial<DefinedInitialDataOptions>
