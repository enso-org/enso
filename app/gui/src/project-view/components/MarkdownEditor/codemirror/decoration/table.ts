import { VueDecorationWidget } from '@/components/MarkdownEditor/codemirror/decoration/vueDecorationWidget'
import { nodeRange } from '@/components/MarkdownEditor/markdown/trees'
import TableEditor from '@/components/MarkdownEditor/TableEditor.vue'
import type { VueHost } from '@/components/VueHostRender.vue'
import type { Text } from '@codemirror/state'
import { Decoration } from '@codemirror/view'
import type { SyntaxNode, SyntaxNodeRef } from '@lezer/common'
import { Range } from 'ydoc-shared/util/data/range'

/** Extension that uses a Vue component CodeMirror widget to render Markdown tables. */
export function decorateTable(
  nodeRef: SyntaxNodeRef,
  doc: Text,
  emitDecoration: (range: Range, deco: Decoration) => void,
  vueHost: VueHost,
) {
  if (nodeRef.name === 'Table') {
    const source = doc //.slice(nodeRef.from, nodeRef.to)
    const parsed = nodeRef.node
    const widget = new TableWidget({ source, parsed }, vueHost)
    emitDecoration(
      nodeRange(nodeRef),
      Decoration.replace({
        widget,
        // Ensure the cursor is drawn relative to the content before the widget.
        // If it is drawn relative to the widget, it will be hidden when the widget is hidden (i.e. during editing).
        side: 1,
        block: true,
      }),
    )
  }
}

class TableWidget extends VueDecorationWidget<{ source: Text; parsed: SyntaxNode }> {
  constructor(props: { source: Text; parsed: SyntaxNode }, vueHost: VueHost) {
    super(TableEditor, props, vueHost, 'cm-table-editor')
  }
}
