/**
 * @file
 *
 * React Query client for the dashboard.
 */

import * as reactQuery from '@tanstack/react-query'

/**
 * Create a new React Query client.
 */
export function createReactQueryClient() {
  return new reactQuery.QueryClient({
    defaultOptions: {
      queries: {
        retry: 3,
      },
    },
  })
}
