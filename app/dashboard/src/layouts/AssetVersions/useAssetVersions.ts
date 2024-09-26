/**
 * @file
 * Fetches the versions of the selected project asset
 */
import * as reactQuery from '@tanstack/react-query'

import type * as backendService from '#/services/Backend'
import type Backend from '#/services/Backend'

/** Parameters for the {@link useAssetVersions} hook. */
export interface UseAssetVersionsParams {
  readonly assetId: backendService.AssetId
  readonly title: string
  readonly backend: Backend
  readonly queryKey?: reactQuery.QueryKey
  readonly enabled?: boolean
  readonly onError?: (error: unknown) => void
}

/** Fetches the versions of the selected project asset. */
export function useAssetVersions(params: UseAssetVersionsParams) {
  const { enabled = true, title, assetId, backend, onError } = params
  const { queryKey = [backend.type, 'listAssetVersions', assetId, title] } = params

  return reactQuery.useQuery({
    queryKey,
    enabled,
    queryFn: () =>
      backend
        .listAssetVersions(assetId, title)
        .then((assetVersions) => assetVersions.versions)
        .catch((backendError) => {
          onError?.(backendError)
          throw backendError
        }),
  })
}
