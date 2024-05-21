import type { NodeCreation } from '@/composables/nodeCreation'
import type { GraphStore, Node, NodeId } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { ToValue } from '@/util/reactivity'
import type { NodeMetadataFields } from 'shared/ast'
import { computed, toValue } from 'vue'

// MIME type in *vendor tree*; see https://www.rfc-editor.org/rfc/rfc6838#section-3.2
// The `web ` prefix is required by Chromium:
// https://developer.chrome.com/blog/web-custom-formats-for-the-async-clipboard-api/.
const ENSO_MIME_TYPE = 'web application/vnd.enso.enso'

/** The data that is copied to the clipboard. */
interface ClipboardData {
  nodes: CopiedNode[]
}

/** Node data that is copied to the clipboard. Used for serializing and deserializing the node information. */
interface CopiedNode {
  expression: string
  documentation?: string | undefined
  metadata?: NodeMetadataFields
}

function nodeStructuredData(node: Node): CopiedNode {
  return {
    expression: node.innerExpr.code(),
    documentation: node.documentation,
    metadata: node.rootExpr.serializeMetadata(),
  }
}

function nodeDataFromExpressionText(expression: string): CopiedNode {
  return { expression }
}

const toTable = computed(() => Pattern.parse('__.to Table'))

/** @internal Exported for testing. */
export function excelTableToEnso(excelData: string) {
  const textLiteral = Ast.TextLiteral.new(excelData)
  return toTable.value.instantiate(textLiteral.module, [textLiteral]).code()
}

/** @internal Exported for testing. */
export async function nodesFromClipboardContent(
  clipboardItems: ClipboardItems,
): Promise<CopiedNode[]> {
  let fallbackItem: ClipboardItem | undefined
  for (const clipboardItem of clipboardItems) {
    for (const type of clipboardItem.types) {
      if (type === ENSO_MIME_TYPE) {
        const blob = await clipboardItem.getType(type)
        return JSON.parse(await blob.text()).nodes
      }

      if (type === 'text/html') {
        const blob = await clipboardItem.getType(type)
        const htmlContent = await blob.text()
        const excelNode = await nodeDataFromExcelClipboard(htmlContent, clipboardItem)
        if (excelNode) {
          return [excelNode]
        }
      }

      if (type === 'text/plain') {
        fallbackItem = clipboardItem
      }
    }
  }
  if (fallbackItem) {
    const fallbackData = await fallbackItem.getType('text/plain')
    return [nodeDataFromExpressionText(await fallbackData.text())]
  }
  return []
}

// Excel data starts with a `table` tag; Google Sheets starts with its own marker.
const spreadsheetHtmlRegex = /^(?:<table |<google-sheets-html-origin>).*<\/table>$/

async function nodeDataFromExcelClipboard(
  htmlContent: string,
  clipboardItem: ClipboardItem,
): Promise<CopiedNode | undefined> {
  // Check if the contents look like HTML tables produced by spreadsheet software known to provide a plain-text
  // version of the table with tab separators, as Excel does.
  if (clipboardItem.types.includes('text/plain') && spreadsheetHtmlRegex.test(htmlContent)) {
    const textData = await clipboardItem.getType('text/plain')
    const expression = excelTableToEnso(await textData.text())
    return nodeDataFromExpressionText(expression)
  }
  return undefined
}

type clipboardItemFactory = (itemData: Record<string, Blob>) => ClipboardItem
type blobFactory = (parts: string[], type: string) => Blob

/** @internal Exported for testing. */
export function nodesToClipboardData(
  nodes: Node[],
  makeClipboardItem: clipboardItemFactory = (data) => new ClipboardItem(data),
  makeBlob: blobFactory = (parts, type) => new Blob(parts, { type }),
): ClipboardItem[] {
  const clipboardData: ClipboardData = { nodes: nodes.map(nodeStructuredData) }
  const jsonItem = makeBlob([JSON.stringify(clipboardData)], ENSO_MIME_TYPE)
  const textItem = makeBlob([nodes.map((node) => node.outerExpr.code()).join('\n')], 'text/plain')
  return [
    makeClipboardItem({
      [jsonItem.type]: jsonItem,
      [textItem.type]: textItem,
    }),
  ]
}

function getClipboard() {
  return (window.navigator as any).mockClipboard ?? window.navigator.clipboard
}

export function useGraphEditorClipboard(
  graphStore: GraphStore,
  selected: ToValue<Set<NodeId>>,
  createNodes: NodeCreation['createNodes'],
) {
  /** Copy the content of the selected node to the clipboard. */
  function copySelectionToClipboard() {
    const nodes = new Array<Node>()
    for (const id of toValue(selected)) {
      const node = graphStore.db.nodeIdToNode.get(id)
      if (!node) continue
      nodes.push(node)
    }
    if (!nodes.length) return
    getClipboard()
      .write(nodesToClipboardData(nodes))
      .catch((error: any) => console.error(`Failed to write to clipboard: ${error}`))
  }

  /** Read the clipboard and if it contains valid data, create nodes from the content. */
  async function createNodesFromClipboard() {
    const clipboardItems = await getClipboard().read()
    const clipboardData = await nodesFromClipboardContent(clipboardItems)
    if (!clipboardData.length) {
      console.warn('No valid node in clipboard.')
      return
    }
    createNodes(
      clipboardData.map(({ expression, documentation, metadata }) => ({
        placement: { type: 'mouse' },
        expression,
        metadata,
        documentation,
      })),
    )
  }

  return {
    copySelectionToClipboard,
    createNodesFromClipboard,
  }
}
