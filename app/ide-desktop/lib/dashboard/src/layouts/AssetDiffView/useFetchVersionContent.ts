/**
 * @file
 *
 * Fetches the content of a project’s Main.enso file with specified version.
 */

import * as reactQuery from '@tanstack/react-query'

import type * as backendService from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

/**
 *
 */
export interface FetchVersionContentProps {
  readonly projectId: backendService.ProjectId
  readonly versionId: string
  readonly backend: RemoteBackend
  readonly omitMetadataFromContent?: boolean
}

/**
 * Fetches the content of a version.
 */
export function useFetchVersionContent(params: FetchVersionContentProps) {
  const { versionId, backend, projectId, omitMetadataFromContent = true } = params

  return reactQuery.useQuery({
    queryKey: ['versionContent', versionId],
    queryFn: () => backend.getFileContent(projectId, versionId),
    select: data => (omitMetadataFromContent ? omitMetadata(data) : data),
  })
}

/**
 * Removes the metadata from the content of a version.
 */
function omitMetadata(file: string): string {
  let [withoutMetadata] = file.split('#### METADATA ####')

  if (withoutMetadata == null) {
    return file
  } else {
    while (withoutMetadata[withoutMetadata.length - 1] === '\n') {
      withoutMetadata = withoutMetadata.slice(0, -1)
    }

    return withoutMetadata
  }
}
