import FrontMatter from '@/components/MarkdownEditor/FrontMatter.vue'
import { nodeRange } from '@/components/MarkdownEditor/markdown/trees'
import { type VueHost } from '@/components/VueHostRender.vue'
import type { Text } from '@codemirror/state'
import { Decoration, WidgetType } from '@codemirror/view'
import type { SyntaxNode, SyntaxNodeRef } from '@lezer/common'
import { h, markRaw } from 'vue'
import { parse } from 'yaml'
import { Range } from 'ydoc-shared/util/data/range'

/** Extension that uses a Vue component CodeMirror widget to render Markdown tables. */
export function decorateFrontMatter(
  nodeRef: SyntaxNodeRef,
  doc: Text,
  emitDecoration: (range: Range, deco: Decoration) => void,
  vueHost: VueHost,
) {
  if (nodeRef.name === 'YAMLFrontMatter') {
    const source = doc.slice(nodeRef.from, nodeRef.to)
    const content = nodeRef.node.getChild('YAMLContent')
    const widget = new FrontMatterWidget({ source, parsed: content! }, vueHost)
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

class FrontMatterWidget extends WidgetType {
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
      container.className = 'cm-frontmatter'
      this.vueHostRegistration = this.vueHost.register(
        () =>
          h(FrontMatter, {
            properties: parse(
              this.props.source.sliceString(this.props.parsed.from, this.props.parsed.to),
            ),
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
