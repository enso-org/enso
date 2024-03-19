/**
 * @file
 */

import * as React from 'react'

import { DiffEditor } from '@monaco-editor/react'

import Spinner, { SpinnerState } from '#/components/Spinner'

import type { ProjectId } from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

import path from './enso.tmLanguage.json?url'
import { useFetchVersionContent } from './useFetchVersionContent'

/**
 *
 */
export interface AssetDiffViewProps {
  readonly versionId: string
  readonly latestVersionId: string
  readonly projectId: ProjectId
  readonly backend: RemoteBackend
}

/**
 * Diff view for asset versions
 */
export function AssetDiffView(props: AssetDiffViewProps) {
  const { versionId, projectId, backend, latestVersionId } = props

  const versionContent = useFetchVersionContent({ versionId, projectId, backend })
  const headContent = useFetchVersionContent({ versionId: latestVersionId, projectId, backend })

  if (versionContent.isError || headContent.isError) {
    return <div className="p-36 text-center">Failed to load content</div>
  }

  if (versionContent.isPending || headContent.isPending) {
    return (
      <div className="h-full w-full flex items-center justify-center">
        <Spinner size={32} state={SpinnerState.loadingMedium} />
      </div>
    )
  }

  return (
    <DiffEditor
      onMount={(editor, monaco) => {
        monaco.languages.register({ id: 'enso', extensions: ['.enso'], configuration: path })
      }}
      original={versionContent.data}
      modified={headContent.data}
      language="enso"
      options={{ readOnly: true }}
    />
  )
}
