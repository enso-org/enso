/** @file Documentation display for an asset. */
import { MarkdownViewer } from '#/components/MarkdownViewer'
import { Result } from '#/components/Result'
import { useLaunchedProjects } from '#/providers/ProjectsProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { isProjectId, type ProjectId } from '#/services/Backend'
import { useStore } from '#/utilities/zustand'
import { useSuspenseQuery } from '@tanstack/react-query'
import { useCallback } from 'react'
import * as ast from 'ydoc-shared/ast'
import { splitFileContents } from 'ydoc-shared/ensoFile'
import { versionContentQueryOptions } from '../../AssetDiffView/useFetchVersionContent'
import { assetPanelStore } from '../AssetPanelState'

/** Props for a {@link AssetDocs}. */
export interface AssetDocsProps {
  readonly backend: Backend
}

/** Documentation display for an asset. */
export function AssetDocs(props: AssetDocsProps) {
  const { backend } = props
  const { getText } = useText()

  const launchedProjects = useLaunchedProjects()

  const { item } = useStore(assetPanelStore, (state) => ({ item: state.assetPanelProps.item }), {
    unsafeEnableTransition: true,
  })

  const launchedProject = launchedProjects[0]
  const launchedProjectId = launchedProject?.hybrid?.cloudProjectId ?? launchedProject?.id

  const itemId = launchedProjectId ?? item?.id

  if (itemId == null) {
    return <Result status="info" title={getText('assetDocs.nothingSelected')} centered />
  }

  if (!isProjectId(itemId)) {
    return <Result status="info" title={getText('assetDocs.notProject')} centered />
  }

  return <AssetDocsContent backend={backend} itemId={itemId} />
}

/** Props for an {@link AssetDocsContent}. */
interface AssetDocsContentProps {
  readonly backend: Backend
  readonly itemId: ProjectId
}

/** Documentation display for an asset. */
export function AssetDocsContent(props: AssetDocsContentProps) {
  const { backend, itemId } = props
  const { getText } = useText()

  const launchedProjects = useLaunchedProjects()

  const { data: docs } = useSuspenseQuery({
    ...versionContentQueryOptions({ backend, projectId: itemId, metadata: false }),
    select: (data) => {
      const { code } = splitFileContents(data)
      const module = ast.parseModule(code)

      for (const statement of module.statements()) {
        if (statement instanceof ast.MutableFunctionDef && statement.name.code() === 'main') {
          return statement.mutableDocumentationMarkdown()
        }
      }

      return ''
    },
  })

  const resolveProjectAssetPath = useCallback(
    (relativePath: string) => backend.resolveProjectAssetPath(itemId, relativePath),
    [backend, itemId],
  )

  if (docs === '') {
    return <Result status="info" title={getText('assetDocs.noDocs')} centered />
  }

  return (
    <MarkdownViewer
      testId="asset-docs"
      toolbar={launchedProjects.length > 0}
      text={docs}
      imgUrlResolver={resolveProjectAssetPath}
    />
  )
}
