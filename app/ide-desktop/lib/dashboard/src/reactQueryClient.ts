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
        retry: (failureCount, error) => {
          // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          const statusesToIgnore = [401, 403, 404]
          const errorStatus =
            'status' in error && typeof error.status === 'number' ? error.status : -1

          if (statusesToIgnore.includes(errorStatus)) {
            return false
          } else {
            return failureCount < 3
          }
        },
      },
    },
  })
}
