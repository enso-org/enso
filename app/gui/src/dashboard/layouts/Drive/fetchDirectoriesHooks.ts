/** @file Hooks related to fetching directories. */
import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
import { useIsMutating, useQuery, useQueryClient } from '@tanstack/react-query'
import type { BackendType } from 'enso-common/src/services/Backend'

/** Periodically refetch directories for the given backend type. */
export function useRefetchDirectories(backendType: BackendType) {
  const queryClient = useQueryClient()
  const enableAssetsTableBackgroundRefresh = useFeatureFlag('enableAssetsTableBackgroundRefresh')
  const assetsTableBackgroundRefreshInterval = useFeatureFlag(
    'assetsTableBackgroundRefreshInterval',
  )
  const isMutating = useIsMutating({ mutationKey: [backendType] }) !== 0

  // We use a different query to refetch the directory data in the background.
  // This reduces the amount of rerenders by batching them together, so they happen less often.
  useQuery({
    queryKey: [backendType, 'refetchListDirectory'],
    queryFn: async () => {
      await queryClient.refetchQueries({
        queryKey: [backendType, 'listDirectory'],
        type: 'active',
      })
      return null
    },
    refetchInterval:
      enableAssetsTableBackgroundRefresh ? assetsTableBackgroundRefreshInterval : false,
    refetchOnMount: 'always',
    refetchIntervalInBackground: false,
    refetchOnWindowFocus: true,
    meta: { persist: false },
    enabled: !isMutating,
  })
}
