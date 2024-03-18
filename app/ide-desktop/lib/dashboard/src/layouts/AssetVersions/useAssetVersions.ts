/**
 * @file
 * Fetches the versions of the selected project asset
 */
import * as reactQuery from '@tanstack/react-query'

import type Backend from '#/services/Backend'
import type * as backendService from '#/services/Backend'

/**
 * Parameters for the useAssetVersions hook
 */
export interface UseAssetVersionsParams {
  readonly assetId: backendService.AssetId
  readonly title: string
  readonly backend: Backend
  readonly queryKey?: reactQuery.QueryKey
  readonly enabled?: boolean
  readonly onError?: (error: unknown) => void
}

/**
 * Fetches the versions of the selected project asset
 */
export function useAssetVersions(params: UseAssetVersionsParams) {
  const {
    enabled = true,
    title,
    assetId,
    backend,
    onError,
    queryKey = ['assetVersions', assetId, title],
  } = params

  return reactQuery.useQuery({
    queryKey,
    queryFn: async () => {
      const assetVersions = await backend.listAssetVersions(assetId, title).catch(backendError => {
        onError?.(backendError)
        throw backendError
      })

      return [
        {
          versionId: 'yVFU12lczU.gpQ_NXY9z4Doete6zxrSd',
          lastModified: '2024-03-12T11:46:26Z',
          isLatest: true,
          key: 'organization-2bdFXZoaETuyLLUAuACmxHeptNv/project-2dZ936qYCt9wwLfva0PCWb0UeON/project_root.enso-project',
        },
        {
          versionId: 'UGF.itWf2t_LbCvvT44rjU0TisC_sV3F',
          lastModified: '2024-03-12T11:14:04Z',
          isLatest: false,
          key: 'organization-2bdFXZoaETuyLLUAuACmxHeptNv/project-2dZ936qYCt9wwLfva0PCWb0UeON/project_root.enso-project',
        },
      ] as backendService.AssetVersions['versions']

      // return assetVersions.versions
    },
    enabled,
  })
}
