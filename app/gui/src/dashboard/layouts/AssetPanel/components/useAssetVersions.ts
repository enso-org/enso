/** @file Fetches the versions of the selected project asset. */

import { backendQueryOptions } from '#/hooks/backendHooks'
import type Backend from '#/services/Backend'
import type { DatalinkId, FileId, ProjectId } from '#/services/Backend'

/** The interval at which the asset versions are refreshed. */
const REFRESH_INTERVAL = 3000 // 3 seconds

/** Options for {@link assetVersionsQueryOptions}. */
export interface AssetVersionsQueryOptions {
  readonly assetId: DatalinkId | FileId | ProjectId
  readonly backend: Backend
  readonly enabled?: boolean
}

/** Options for a query that fetches the versions of an asset. */
export function assetVersionsQueryOptions(options: AssetVersionsQueryOptions) {
  const { enabled = true, assetId, backend } = options

  return backendQueryOptions(backend, 'listAssetVersions', [assetId], {
    enabled,
    refetchIntervalInBackground: true,
    refetchOnWindowFocus: 'always',
    refetchInterval: REFRESH_INTERVAL,
  })
}
