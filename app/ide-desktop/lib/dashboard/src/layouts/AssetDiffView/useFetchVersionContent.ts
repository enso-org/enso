/** @file Fetches the content of a project’s Main.enso file with specified version. */

import * as reactQuery from '@tanstack/react-query'

import type * as backendService from '#/services/Backend'
import type Backend from '#/services/Backend'

// =================
// === Constants ===
// =================

const MS_IN_SECOND = 1000
const HUNDRED = 100
const HUNDRED_SECONDS = HUNDRED * MS_IN_SECOND

// ==============================
// === useFetchVersionContent ===
// ==============================

/** Options for {@link useFetchVersionContent}. */
export interface FetchVersionContentOptions {
  readonly project: backendService.ProjectAsset
  readonly versionId: string
  readonly backend: Backend
  /** If `false`, the metadata is stripped out. Defaults to `false`. */
  readonly metadata?: boolean
}

/** Fetch the content of a version. */
export function useFetchVersionContent(params: FetchVersionContentOptions) {
  const { versionId, backend, project, metadata = false } = params

  return reactQuery.useQuery({
    queryKey: ['versionContent', versionId],
    queryFn: () => backend.getFileContent(project.id, versionId, project.title),
    select: data => (metadata ? data : omitMetadata(data)),
    staleTime: HUNDRED_SECONDS,
  })
}

/** Remove the metadata from the content of a version. */
function omitMetadata(file: string): string {
  return file.split('#### METADATA ####')[0]?.replace(/\n+$/, '') ?? file
}
