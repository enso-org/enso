import TableEditor from '@/components/MarkdownEditor/TableEditor.vue'
import { type VueHost } from '@/components/VueHostRender.vue'
import type { Text } from '@codemirror/state'
import { Decoration, WidgetType } from '@codemirror/view'
import type { SyntaxNode, SyntaxNodeRef } from '@lezer/common'
import { h, markRaw } from 'vue'

/** Extension that uses a Vue component CodeMirror widget to render Markdown tables. */
export function decorateTable(
  nodeRef: SyntaxNodeRef,
  doc: Text,
  emitDecoration: (from: number, to: number, deco: Decoration) => void,
  vueHost: VueHost,
) {
  if (nodeRef.name === 'Table') {
    const source = doc //.slice(nodeRef.from, nodeRef.to)
    const parsed = nodeRef.node
    const widget = new TableWidget({ source, parsed }, vueHost)
    emitDecoration(
      nodeRef.from,
      nodeRef.to,
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

class TableWidget extends WidgetType {
  private container: HTMLElement | undefined
  private vueHostRegistration: { unregister: () => void } | undefined

  constructor(
    private readonly props: { source: Text; parsed: SyntaxNode },
    private readonly vueHost: VueHost,
  ) {
    super()
  }

  override get estimatedHeight() {
    return -1
  }

  override toDOM(): HTMLElement {
    if (!this.container) {
      const container = markRaw(document.createElement('div'))
      container.className = 'cm-table-editor'
      this.vueHostRegistration = this.vueHost.register(
        () =>
          h(TableEditor, {
            source: this.props.source,
            parsed: this.props.parsed,
            onEdit: () => console.log('onEdit'),
          }),
        container,
      )
      this.container = container
    }
    return this.container
  }

  override destroy() {
    this.vueHostRegistration?.unregister()
    this.container = undefined
  }
}
