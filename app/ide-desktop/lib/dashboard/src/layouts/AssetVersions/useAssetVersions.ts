/** @file Fetch the versions of the selected project asset. */
import * as reactQuery from '@tanstack/react-query'

import type * as backendService from '#/services/Backend'

/** Parameters for the {@link useAssetVersions} hook. */
export interface UseAssetVersionsParams {
  readonly item: backendService.AnySmartAsset
  readonly queryKey?: reactQuery.QueryKey
  readonly enabled?: boolean
  readonly onError?: (error: unknown) => void
}

/** Fetch the versions of the selected project asset. */
export function useAssetVersions(params: UseAssetVersionsParams) {
  const { item, onError, queryKey = ['assetVersions', item.value.id] } = params

  return reactQuery.useQuery({
    queryKey,
    queryFn: () =>
      item
        .listVersions()
        .then(versions => versions.versions)
        .catch(backendError => {
          onError?.(backendError)
          throw backendError
        }),
  })
}
