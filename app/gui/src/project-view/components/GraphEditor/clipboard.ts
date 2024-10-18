import type { NodeCreationOptions } from '@/composables/nodeCreation'
import type { GraphStore, Node, NodeId } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { filterDefined } from '@/util/data/iterable'
import { Vec2 } from '@/util/data/vec2'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue } from 'vue'
import type { NodeMetadataFields } from 'ydoc-shared/ast'

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
  binding?: string
  documentation?: string | undefined
  metadata?: NodeMetadataFields
}

/** @internal */
export async function nodesFromClipboardContent(
  clipboardItems: ClipboardItems,
): Promise<CopiedNode[]> {
  return [
    ...(await decodeClipboard(clipboardItems, [ensoDecoder, spreadsheetDecoder, plainTextDecoder])),
  ].flat()
}

const ensoDecoder: ClipboardDecoder<CopiedNode[]> = {
  mimeType: ENSO_MIME_TYPE,
  decode: async (blob) => (JSON.parse(await blob.text()) as ClipboardData).nodes,
}
const plainTextDecoder: ClipboardDecoder<CopiedNode[]> = {
  mimeType: 'text/plain',
  decode: async (blob) => [{ expression: await blob.text() }],
}

interface ExtendedClipboard extends Clipboard {
  // Recent addition to the spec: https://github.com/w3c/clipboard-apis/pull/197
  // Currently supported by Chromium: https://developer.chrome.com/docs/web-platform/unsanitized-html-async-clipboard
  read(options?: { unsanitized?: ['text/html'] }): Promise<ClipboardItems>
}

function getClipboard(): ExtendedClipboard {
  return (window.navigator as any).mockClipboard ?? window.navigator.clipboard
}

/** A composable for handling copying and pasting nodes in the GraphEditor. */
export function useGraphEditorClipboard(
  graphStore: GraphStore,
  selected: ToValue<Set<NodeId>>,
  createNodes: (nodesOptions: Iterable<NodeCreationOptions>) => void,
) {
  /** Copy the content of the selected node to the clipboard. */
  async function copySelectionToClipboard() {
    const nodes = new Array<Node>()
    const ids = graphStore.pickInCodeOrder(toValue(selected))
    for (const id of ids) {
      const node = graphStore.db.nodeIdToNode.get(id)
      if (!node) continue
      nodes.push(node)
    }
    if (!nodes.length) return
    return writeClipboard(nodesToClipboardData(nodes))
  }

  /** Read the clipboard and if it contains valid data, create nodes from the content. */
  async function createNodesFromClipboard() {
    const clipboardItems = await getClipboard().read({
      // Chromium-based browsers support reading unsanitized HTML data, so we can obtain predictable data for
      // spreadsheet recognition in that case; other browsers, including Firefox (as of v127), do not, and should have
      // their sanitized data included in test cases in `clipboardTestCases.json`.
      unsanitized: ['text/html'],
    })
    const clipboardData = await nodesFromClipboardContent(clipboardItems)
    if (!clipboardData.length) {
      console.warn('No valid node in clipboard.')
      return
    }
    const firstNodePos = clipboardData[0]!.metadata?.position ?? { x: 0, y: 0 }
    const originPos = new Vec2(firstNodePos.x, firstNodePos.y)
    createNodes(
      clipboardData.map(({ expression, binding, documentation, metadata }) => {
        const pos = metadata?.position
        const relativePos = pos ? new Vec2(pos.x, pos.y).sub(originPos) : new Vec2(0, 0)
        return {
          placement: { type: 'mouseRelative', posOffset: relativePos },
          expression,
          binding,
          metadata,
          documentation,
        }
      }),
    )
  }

  return {
    copySelectionToClipboard,
    createNodesFromClipboard,
  }
}

// ==========================
// === Clipboard decoding ===
// ==========================

interface ClipboardDecoder<T> {
  mimeType: string
  decode: (blob: Blob, item: ClipboardItem) => Promise<T | undefined>
}

async function decodeClipboard<T>(
  clipboardItems: ClipboardItems,
  decoders: ClipboardDecoder<T>[],
): Promise<IterableIterator<T>> {
  const decodeItem = async (clipboardItem: ClipboardItem) => {
    for (const decoder of decoders) {
      if (clipboardItem.types.includes(decoder.mimeType)) {
        const blob = await clipboardItem.getType(decoder.mimeType)
        const decoded = await decoder.decode(blob, clipboardItem)
        if (decoded) return decoded
      }
    }
  }
  return filterDefined(await Promise.all(clipboardItems.map(decodeItem)))
}

// === Spreadsheet clipboard decoder ===

const spreadsheetDecoder: ClipboardDecoder<CopiedNode[]> = {
  mimeType: 'text/html',
  decode: async (blob, item) => {
    const htmlContent = await blob.text()
    if (!item.types.includes('text/plain')) return
    if (isSpreadsheetTsv(htmlContent)) {
      const textData = await item.getType('text/plain').then((blob) => blob.text())
      return [{ expression: tsvTableToEnsoExpression(textData) }]
    }
  },
}

const toTable = computed(() => Pattern.parse('__.to Table'))

/** Create Enso Expression generating table from this tsvData. */
export function tsvTableToEnsoExpression(tsvData: string) {
  const textLiteral = Ast.TextLiteral.new(tsvData)
  return toTable.value.instantiate(textLiteral.module, [textLiteral]).code()
}

/** @internal */
export function isSpreadsheetTsv(htmlContent: string) {
  // This is a very general criterion that can have some false-positives (e.g. pasting rich text that includes a table).
  // However, due to non-standardized browser HTML sanitization it is difficult to precisely recognize spreadsheet
  // clipboard data. We want to avoid false negatives (even if a browser changes its sanitization), and in case of a
  // false positive the user is pasting data we don't have any good way to handle, so trying to make a Table from it is
  // acceptable.
  return /<table[ >]/i.test(htmlContent)
}

// =========================
// === Clipboard writing ===
// =========================

export type MimeType = 'text/plain' | 'text/html' | typeof ENSO_MIME_TYPE
export type MimeData = Partial<Record<MimeType, string>>

/** Write data to clipboard */
export function writeClipboard(data: MimeData) {
  const dataBlobs = Object.fromEntries(
    Object.entries(data).map(([type, typeData]) => [type, new Blob([typeData], { type })]),
  )
  return getClipboard()
    .write([new ClipboardItem(dataBlobs)])
    .catch((error: any) => console.error(`Failed to write to clipboard: ${error}`))
}

// === Serializing nodes ===

function nodeStructuredData(node: Node): CopiedNode {
  return {
    expression: node.innerExpr.code(),
    documentation: node.docs?.documentation(),
    metadata: node.rootExpr.serializeMetadata(),
    ...(node.pattern ? { binding: node.pattern.code() } : {}),
  }
}

/** TODO: Add docs */
export function clipboardNodeData(nodes: CopiedNode[]): MimeData {
  const clipboardData: ClipboardData = { nodes }
  return { [ENSO_MIME_TYPE]: JSON.stringify(clipboardData) }
}

/** TODO: Add docs */
export function nodesToClipboardData(nodes: Node[]): MimeData {
  return {
    ...clipboardNodeData(nodes.map(nodeStructuredData)),
    'text/plain': nodes.map((node) => node.outerExpr.code()).join('\n'),
  }
}
