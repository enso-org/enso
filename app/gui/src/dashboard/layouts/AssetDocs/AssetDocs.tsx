/**
 * @file
 * Display the docs for an asset.
 */
import { MarkdownViewer } from '#/components/MarkdownViewer'
import { Result } from '#/components/Result'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { AssetType, type ProjectAsset } from '#/services/Backend'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import { useSuspenseQuery } from '@tanstack/react-query'
import * as ast from 'ydoc-shared/ast'
import { Tree } from 'ydoc-shared/ast/generated/ast'
import { splitFileContents } from 'ydoc-shared/ensoFile'
import { versionContentQueryOptions } from '../AssetDiffView/useFetchVersionContent'

/**
 * Props for a {@link AssetDocs}.
 */
export interface AssetDocsProps {
  readonly backend: Backend
  readonly item: AssetTreeNode | null
}

/**
 * Display the docs for an asset.
 */
export function AssetDocs(props: AssetDocsProps) {
  const { backend, item } = props

  const { getText } = useText()

  if (item?.item.type !== AssetType.project) {
    return <Result status="info" title={getText('assetDocs.notProject')} centered />
  }

  // This is safe because we already checked that the item is a project above.
  // eslint-disable-next-line no-restricted-syntax
  return <AssetDocsContent backend={backend} item={item as AssetTreeNode<ProjectAsset>} />
}

/**
 * Props for {@link AssetDocsContent}.
 */
interface AssetDocsContentProps {
  readonly backend: Backend
  readonly item: AssetTreeNode<ProjectAsset>
}

/**
 * Display the docs for an asset.
 */
export function AssetDocsContent(props: AssetDocsContentProps) {
  const { backend, item } = props
  const { getText } = useText()

  const { data: docs } = useSuspenseQuery({
    ...versionContentQueryOptions({ backend, projectId: item.item.id, metadata: false }),
    select: (data) => {
      const withoutMeta = splitFileContents(data)
      // We use the raw parser here because we don't need the whole AST, only the Docs part,
      // we skip parsing the whole file, which is a lot faster by the time of writing this (5-10 times).
      const tree = ast.rawParseModule(withoutMeta.code)

      for (const node of tree.statements) {
        if (node.expression?.type === Tree.Type.Documented) {
          const module = ast.MutableModule.Transient()
          const x = ast.abstract(module, node.expression, data)
          return x.root.documentingAncestor()?.documentation() ?? ''
        }
      }

      return ''
    },
  })

  if (docs === '') {
    return <Result status="info" title={getText('assetDocs.noDocs')} centered />
  }

  return <MarkdownViewer text={docs} />
}
