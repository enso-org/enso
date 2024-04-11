import type { NavigatorComposable } from '@/composables/navigator'
import type { GraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { Vec2 } from '@/util/data/vec2'
import type { NodeMetadataFields } from 'shared/ast'

const ENSO_MIME_TYPE = 'web application/enso'

/** The data that is copied to the clipboard. */
interface ClipboardData {
  nodes: CopiedNode[]
}

/** Node data that is copied to the clipboard. Used for serializing and deserializing the node information. */
interface CopiedNode {
  expression: string
  metadata: NodeMetadataFields | undefined
}

export function useGraphEditorClipboard(
  nodeSelection: GraphSelection,
  graphNavigator: NavigatorComposable,
) {
  const graphStore = useGraphStore()

  /** Copy the content of the selected node to the clipboard. */
  function copyNodeContent() {
    const id = nodeSelection.selected.values().next().value
    const node = graphStore.db.nodeIdToNode.get(id)
    if (!node) return
    const content = node.innerExpr.code()
    const nodeMetadata = node.rootExpr.nodeMetadata
    const metadata = {
      position: nodeMetadata.get('position'),
      visualization: nodeMetadata.get('visualization'),
    }
    const copiedNode: CopiedNode = { expression: content, metadata }
    const clipboardData: ClipboardData = { nodes: [copiedNode] }
    const jsonItem = new Blob([JSON.stringify(clipboardData)], { type: ENSO_MIME_TYPE })
    const textItem = new Blob([content], { type: 'text/plain' })
    const clipboardItem = new ClipboardItem({
      [jsonItem.type]: jsonItem,
      [textItem.type]: textItem,
    })
    navigator.clipboard.write([clipboardItem])
  }

  async function retrieveDataFromClipboard(): Promise<ClipboardData | undefined> {
    const clipboardItems = await navigator.clipboard.read()
    let fallback = undefined
    for (const clipboardItem of clipboardItems) {
      for (const type of clipboardItem.types) {
        if (type === ENSO_MIME_TYPE) {
          const blob = await clipboardItem.getType(type)
          return JSON.parse(await blob.text())
        }

        if (type === 'text/html') {
          const blob = await clipboardItem.getType(type)
          const htmlContent = await blob.text()
          const excelPayload = await readNodeFromExcelClipboard(htmlContent, clipboardItem)
          if (excelPayload) {
            return excelPayload
          }
        }

        if (type === 'text/plain') {
          const blob = await clipboardItem.getType(type)
          const fallbackExpression = await blob.text()
          const fallbackNode = { expression: fallbackExpression, metadata: undefined } as CopiedNode
          fallback = { nodes: [fallbackNode] } as ClipboardData
        }
      }
    }
    return fallback
  }

  /// Read the clipboard and if it contains valid data, create a node from the content.
  async function readNodeFromClipboard() {
    const clipboardData = await retrieveDataFromClipboard()
    const copiedNode = clipboardData?.nodes[0]
    if (!copiedNode) {
      console.warn('No valid node in clipboard.')
      return
    }
    if (copiedNode.expression == null) {
      console.warn('No valid expression in clipboard.')
    }
    graphStore.createNode(
      graphNavigator.sceneMousePos ?? Vec2.Zero,
      copiedNode.expression,
      copiedNode.metadata,
    )
  }

  async function readNodeFromExcelClipboard(
    htmlContent: string,
    clipboardItem: ClipboardItem,
  ): Promise<ClipboardData | undefined> {
    // Check we have a valid HTML table
    // If it is Excel, we should have a plain-text version of the table with tab separators.
    if (
      clipboardItem.types.includes('text/plain') &&
      htmlContent.startsWith('<table ') &&
      htmlContent.endsWith('</table>')
    ) {
      const textData = await clipboardItem.getType('text/plain')
      const text = await textData.text()
      const payload = JSON.stringify(text).replaceAll(/^"|"$/g, '').replaceAll("'", "\\'")
      const expression = `'${payload}'.to Table`
      return { nodes: [{ expression: expression, metadata: undefined }] } as ClipboardData
    }
    return undefined
  }

  return {
    copyNodeContent,
    readNodeFromClipboard,
  }
}
