/** @file Diff view comparing `Main.enso` of two versions for a specific project. */
import { DiffEditor } from '@monaco-editor/react'
import { useSuspenseQueries } from '@tanstack/react-query'

import { StatelessSpinner } from '#/components/StatelessSpinner'
import type * as backendService from 'enso-common/src/services/Backend'
import type { Backend } from 'enso-common/src/services/Backend'
import { versionContentQueryOptions } from './queries'

/** Props for an {@link AssetDiffView}. */
export interface AssetDiffViewProps {
  readonly currentVersionId: backendService.S3ObjectVersionId | undefined
  readonly previousVersionId: backendService.S3ObjectVersionId | undefined
  readonly project: backendService.ProjectAsset
  readonly backend: Backend
}

/** Diff view comparing `Main.enso` of two versions for a specific project. */
export function AssetDiffView(props: AssetDiffViewProps) {
  const { currentVersionId, previousVersionId, project, backend } = props

  const [currentVersionContent, previousVersionContent] = useSuspenseQueries({
    queries: [
      currentVersionId ?
        versionContentQueryOptions({
          versionId: currentVersionId,
          projectId: project.id,
          backend,
        })
      : undefined,
      previousVersionId ?
        versionContentQueryOptions({
          versionId: previousVersionId,
          projectId: project.id,
          backend,
        })
      : undefined,
    ].filter((query) => query !== undefined),
  })

  const loader = (
    <div className="flex h-full w-full items-center justify-center">
      <StatelessSpinner size={32} phase="loading-medium" />
    </div>
  )

  return (
    <DiffEditor
      beforeMount={(monaco) => {
        monaco.editor.defineTheme('transparentBackground', {
          base: 'vs',
          inherit: true,
          rules: [],
          // The name comes from a third-party API and cannot be changed.
          // eslint-disable-next-line @typescript-eslint/naming-convention
          colors: { 'editor.background': '#00000000' },
        })
      }}
      original={previousVersionContent?.data ?? ''}
      modified={currentVersionContent?.data ?? ''}
      language="enso"
      options={{ readOnly: true }}
      loading={loader}
      theme="transparentBackground"
    />
  )
}
