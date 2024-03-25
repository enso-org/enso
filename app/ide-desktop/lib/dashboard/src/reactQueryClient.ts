/**
 * @file
 *
 * React Query client for the dashboard.
 */

import * as reactQuery from '@tanstack/react-query'

import * as httpClientModule from '#/utilities/HttpClient'

const NO_RETRY_MIN_STATUS = 300
const NO_RETRY_MAX_STATUS = 599

/**
 * Create a new React Query client.
 */
export function createReactQueryClient() {
  return new reactQuery.QueryClient({
    defaultOptions: {
      queries: {
        retry: (failureCount, error) => {
          if (
            error instanceof httpClientModule.NetworkError &&
            error.status != null &&
            error.status >= NO_RETRY_MIN_STATUS &&
            error.status <= NO_RETRY_MAX_STATUS
          ) {
            return false
          } else {
            return failureCount < 3
          }
        },
      },
    },
  })
}
