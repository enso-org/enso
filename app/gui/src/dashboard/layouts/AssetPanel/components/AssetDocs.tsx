/** @file Documentation display for an asset. */
import { Result } from '#/components/Result'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import type { Asset } from '#/services/Backend'
import { AssetType } from '#/services/Backend'
import { useStore } from '#/utilities/zustand'
import { useSuspenseQuery } from '@tanstack/react-query'
import { lazy, useCallback } from 'react'
import * as ast from 'ydoc-shared/ast'
import { splitFileContents } from 'ydoc-shared/ensoFile'
import { assetPanelStore } from '../AssetPanelState'
import { versionContentQueryOptions } from './queries'
import type { AssetPanelProps } from './types'

/** Props for a {@link AssetDocs}. */
export interface AssetDocsProps extends AssetPanelProps {}

const LazyMarkdownViewer = lazy(() =>
  import('#/components/MarkdownViewer').then((module) => ({ default: module.MarkdownViewer })),
)

/** Documentation display for an asset. */
export function AssetDocs(props: AssetDocsProps) {
  const { backend } = props
  const { getText } = useText()

  const item = useStore(
    assetPanelStore,
    (state) => state.assetPanelProps.item ?? state.assetPanelProps.defaultItem,
    { unsafeEnableTransition: true, areEqual: 'shallow' },
  )

  if (item?.type !== AssetType.project) {
    return <Result status="info" title={getText('assetDocs.notProject')} centered />
  }

  return <AssetDocsContent backend={backend} item={item} />
}

/** Props for an {@link AssetDocsContent}. */
interface AssetDocsContentProps {
  readonly backend: Backend
  readonly item: Asset<AssetType.project>
}

/** Documentation display for an asset. */
export function AssetDocsContent(props: AssetDocsContentProps) {
  const { backend, item } = props
  const { getText } = useText()

  const { data: docs } = useSuspenseQuery({
    ...versionContentQueryOptions({ backend, projectId: item.id, metadata: false }),
    select: (data) => {
      const { code } = splitFileContents(data)
      const module = ast.parseModule(code)

      for (const statement of module.statements()) {
        if (statement instanceof ast.MutableFunctionDef && statement.name.code() === 'main') {
          return statement.mutableDocumentationMarkdown().toJSON()
        }
      }

      return ''
    },
  })

  const resolveProjectAssetPath = useCallback(
    (relativePath: string) => backend.resolveProjectAssetPath(item.id, relativePath),
    [backend, item.id],
  )

  if (!docs) {
    return <Result status="info" title={getText('assetDocs.noDocs')} centered />
  }

  return (
    <LazyMarkdownViewer testId="asset-docs" text={docs} imgUrlResolver={resolveProjectAssetPath} />
  )
}
