import type { LexicalPlugin } from '@/components/lexical'
import { useLexicalStringSync } from '@/components/lexical/sync'
import { CodeHighlightNode, CodeNode } from '@lexical/code'
import { AutoLinkNode, LinkNode } from '@lexical/link'
import { ListItemNode, ListNode } from '@lexical/list'
import {
  $convertFromMarkdownString,
  $convertToMarkdownString,
  TRANSFORMERS,
  registerMarkdownShortcuts,
  type Transformer,
} from '@lexical/markdown'
import { HeadingNode, QuoteNode, registerRichText } from '@lexical/rich-text'
import { TableCellNode, TableNode, TableRowNode } from '@lexical/table'
import { $setSelection } from 'lexical'
import { watch, type Ref } from 'vue'

export interface LexicalMarkdownPlugin extends LexicalPlugin {
  transformers?: Transformer[]
}

/** TODO: Add docs */
export function markdownPlugin(
  model: Ref<string>,
  extensions: LexicalMarkdownPlugin[],
): LexicalPlugin[] {
  const transformers = new Array<Transformer>()
  for (const extension of extensions) {
    if (extension?.transformers) transformers.push(...extension.transformers)
  }
  transformers.push(...TRANSFORMERS)
  return [...extensions, baseMarkdownPlugin(transformers), markdownSyncPlugin(model, transformers)]
}

function baseMarkdownPlugin(transformers: Transformer[]): LexicalPlugin {
  return {
    nodes: [
      HeadingNode,
      QuoteNode,
      ListItemNode,
      ListNode,
      AutoLinkNode,
      LinkNode,
      CodeHighlightNode,
      CodeNode,
      TableCellNode,
      TableNode,
      TableRowNode,
    ],
    register: (editor) => {
      registerRichText(editor)
      registerMarkdownShortcuts(editor, transformers)
    },
  }
}

const markdownSyncPlugin = (model: Ref<string>, transformers: Transformer[]): LexicalPlugin => ({
  register: (editor) => {
    const { content } = useLexicalStringSync(
      editor,
      () => $convertToMarkdownString(transformers),
      (value) => {
        $convertFromMarkdownString(value, transformers)
        $setSelection(null)
      },
    )
    watch(model, (newContent) => content.set(newContent), { immediate: true })
    watch(content.editedContent, (newContent) => (model.value = newContent))
  },
})
