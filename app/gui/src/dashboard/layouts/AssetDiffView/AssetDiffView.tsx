/** @file Diff view comparing `Main.enso` of two versions for a specific project. */
import * as monacoReact from '@monaco-editor/react'

import * as textProvider from '#/providers/TextProvider'

import Spinner, * as spinnerModule from '#/components/Spinner'

import type * as backendService from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as useFetchVersionContent from './useFetchVersionContent'

// =====================
// === AssetDiffView ===
// =====================

/** Props for an {@link AssetDiffView}. */
export interface AssetDiffViewProps {
  readonly versionId: string
  readonly latestVersionId: string
  readonly project: backendService.ProjectAsset
  readonly backend: Backend
}

/** Diff view comparing `Main.enso` of two versions for a specific project. */
export function AssetDiffView(props: AssetDiffViewProps) {
  const { versionId, project, backend, latestVersionId } = props
  const { getText } = textProvider.useText()

  const versionContent = useFetchVersionContent.useFetchVersionContent({
    versionId,
    project,
    backend,
  })
  const headContent = useFetchVersionContent.useFetchVersionContent({
    versionId: latestVersionId,
    project,
    backend,
  })

  const loader = (
    <div className="flex h-full w-full items-center justify-center">
      <Spinner size={32} state={spinnerModule.SpinnerState.loadingMedium} />
    </div>
  )

  if (versionContent.isError || headContent.isError) {
    return <div className="p-indent-8 text-center">{getText('loadFileError')}</div>
  } else if (versionContent.isPending || headContent.isPending) {
    return loader
  } else {
    return (
      <monacoReact.DiffEditor
        beforeMount={(monaco) => {
          monaco.editor.defineTheme('myTheme', {
            base: 'vs',
            inherit: true,
            rules: [],
            // The name comes from a third-party API and cannot be changed.
            // eslint-disable-next-line @typescript-eslint/naming-convention
            colors: { 'editor.background': '#00000000' },
          })
        }}
        original={versionContent.data}
        modified={headContent.data}
        language="enso"
        options={{ readOnly: true }}
        loading={loader}
        theme={'myTheme'}
      />
    )
  }
}
