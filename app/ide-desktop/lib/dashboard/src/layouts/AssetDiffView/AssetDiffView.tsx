/**
 * @file
 *
 * Diff view for 2 asset versions for a specific project
 */
import * as react from '@monaco-editor/react'

import Spinner, * as spinnerModule from '#/components/Spinner'

import type * as backendService from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as useFetchVersionContent from './useFetchVersionContent'

/**
 * Props for the AssetDiffView component
 */
export interface AssetDiffViewProps {
  readonly versionId: string
  readonly latestVersionId: string
  readonly projectId: backendService.ProjectId
  readonly backend: RemoteBackend
}

/**
 * Diff view for asset versions
 */
export function AssetDiffView(props: AssetDiffViewProps) {
  const { versionId, projectId, backend, latestVersionId } = props

  const versionContent = useFetchVersionContent.useFetchVersionContent({
    versionId,
    projectId,
    backend,
  })
  const headContent = useFetchVersionContent.useFetchVersionContent({
    versionId: latestVersionId,
    projectId,
    backend,
  })

  if (versionContent.isError || headContent.isError) {
    return <div className="p-indent-8 text-center">Failed to load content</div>
  } else if (versionContent.isPending || headContent.isPending) {
    return (
      <div className="flex h-full w-full items-center justify-center">
        <Spinner size={32} state={spinnerModule.SpinnerState.loadingMedium} />
      </div>
    )
  } else
    return (
      <react.DiffEditor
        original={versionContent.data}
        modified={headContent.data}
        language="enso"
        options={{ readOnly: true }}
      />
    )
}
