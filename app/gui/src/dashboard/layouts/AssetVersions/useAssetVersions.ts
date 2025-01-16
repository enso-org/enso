/**
 * @file
 * Fetches the versions of the selected project asset
 */
import type Backend from '#/services/Backend'
import type { AssetId } from '#/services/Backend'
import { queryOptions, useQuery } from '@tanstack/react-query'

/**
 * Options for {@link useAssetVersions}.
 */
export interface AssetVersionsQueryOptions {
  readonly assetId: AssetId
  readonly backend: Backend
  readonly enabled?: boolean
  readonly onError?: ((error: unknown) => void) | undefined
}

/** Fetches the versions of the selected project asset. */
export function useAssetVersions(params: AssetVersionsQueryOptions) {
  const { enabled = true, assetId, backend, onError } = params

  return useQuery(assetVersionsQueryOptions({ assetId, backend, enabled, onError }))
}

/**
 * Options for a query that fetches the versions of an asset.
 */
export function assetVersionsQueryOptions(params: AssetVersionsQueryOptions) {
  const { enabled = true, assetId, backend, onError } = params

  return queryOptions({
    queryKey: [backend.type, 'listAssetVersions', { assetId }] as const,
    enabled,
    queryFn: ({ queryKey: [, , props] }) =>
      backend
        .listAssetVersions(props.assetId)
        .then((assetVersions) => assetVersions.versions)
        .catch((backendError) => {
          onError?.(backendError)
          throw backendError
        }),
  })
}
