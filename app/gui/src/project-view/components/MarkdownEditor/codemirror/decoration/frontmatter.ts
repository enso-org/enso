import { parseMetadata, type DocumentationMetadata } from '@/components/ComponentHelp/metadata'
import { VueDecorationWidget } from '@/components/MarkdownEditor/codemirror/decoration/vueDecorationWidget'
import FrontMatter from '@/components/MarkdownEditor/FrontMatter.vue'
import { nodeRange } from '@/components/MarkdownEditor/markdown/trees'
import type { VueHost } from '@/components/VueHostRender.vue'
import type { Text } from '@codemirror/state'
import { Decoration } from '@codemirror/view'
import type { SyntaxNodeRef } from '@lezer/common'
import { Range } from 'ydoc-shared/util/data/range'

/** Extension that uses a Vue component CodeMirror widget to render documentation metadata. */
export function decorateFrontMatter(
  nodeRef: SyntaxNodeRef,
  doc: Text,
  emitDecoration: (range: Range, deco: Decoration) => void,
  vueHost: VueHost,
) {
  if (nodeRef.name !== 'YAMLFrontMatter') return
  const content = nodeRef.node.getChild('YAMLContent')
  if (!content) return
  const res = parseMetadata(doc.sliceString.bind(doc), content)
  if (!res.ok) {
    console.error('Invalid documentation metadata, parsing failed with error: ', res.error)
  } else {
    const widget = new FrontMatterWidget({ metadata: res.value }, vueHost)
    emitDecoration(
      nodeRange(nodeRef),
      Decoration.replace({
        widget,
        block: true,
      }),
    )
  }
}

class FrontMatterWidget extends VueDecorationWidget<{ metadata: DocumentationMetadata }> {
  constructor(props: { metadata: DocumentationMetadata }, vueHost: VueHost) {
    super(FrontMatter, props, vueHost, 'cm-frontmatter')
  }
}
