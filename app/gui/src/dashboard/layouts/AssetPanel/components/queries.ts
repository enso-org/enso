/** @file Fetches the versions of the selected project asset. */

import { backendQueryOptions } from '#/hooks/backendHooks'
import { queryOptions, useQuery } from '@tanstack/react-query'
import type {
  AssetId,
  Backend,
  DatalinkId,
  FileId,
  ProjectId,
  S3ObjectVersionId,
} from 'enso-common/src/services/Backend'
import { splitFileContents } from 'ydoc-shared/ensoFile'

/** Options for {@link assetVersionsQueryOptions}. */
export interface AssetVersionsQueryOptions {
  readonly assetId: DatalinkId | FileId | ProjectId
  readonly backend: Backend
  readonly enabled?: boolean
}

/** Options for a query that fetches the versions of an asset. */
export function assetVersionsQueryOptions(options: AssetVersionsQueryOptions) {
  const { enabled = true, assetId, backend } = options

  return backendQueryOptions(backend, 'listAssetVersions', [assetId], { enabled })
}

/** Options for a query that fetches the details of an asset. */
export interface AssetDetailsQueryOptions {
  readonly assetId?: AssetId | null | undefined
  readonly backend: Backend
  readonly enabled?: boolean
  readonly refetchInterval?: number
}

/** Options for {@link useFetchVersionContent}. */
export interface FetchVersionContentOptions {
  readonly projectId: ProjectId
  readonly versionId?: S3ObjectVersionId
  readonly backend: Backend
  /** If `false`, the metadata is stripped out. Defaults to `false`. */
  readonly metadata?: boolean
}

/**
 * Return the query options for fetching the content of a version.
 */
export function versionContentQueryOptions(params: FetchVersionContentOptions) {
  return queryOptions({
    queryKey: [
      params.backend.type,
      {
        method: 'getFileContent',
        versionId: params.versionId,
        projectId: params.projectId,
      },
    ] as const,
    queryFn: ({ queryKey }) => {
      const [, { versionId, projectId }] = queryKey
      return params.backend.getMainFileContent(projectId, versionId)
    },
    select: (data) => (params.metadata === true ? data : omitMetadata(data)),
  })
}

/** Fetch the content of a version. */
export function useFetchVersionContent(params: FetchVersionContentOptions) {
  return useQuery(versionContentQueryOptions(params))
}

/** Remove the metadata from the content of a version. */
function omitMetadata(file: string): string {
  return splitFileContents(file).code
}
