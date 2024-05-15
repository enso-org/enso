<script setup lang="ts">
import TextEditor from '@/components/lexical/TextEditor.vue'
import { useLexicalSync } from '@/components/lexical/sync'
import { CodeHighlightNode, CodeNode } from '@lexical/code'
import { AutoLinkNode, LinkNode } from '@lexical/link'
import { ListItemNode, ListNode } from '@lexical/list'
import {
  $convertFromMarkdownString,
  $convertToMarkdownString,
  TRANSFORMERS,
  registerMarkdownShortcuts,
} from '@lexical/markdown'
import { HeadingNode, QuoteNode, registerRichText } from '@lexical/rich-text'
import { TableCellNode, TableNode, TableRowNode } from '@lexical/table'
import { syncRef } from '@vueuse/core'
import { type LexicalEditor } from 'lexical'

const markdown = defineModel<string>({ required: true })

const nodes = [
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
]

function configure(editor: LexicalEditor) {
  registerRichText(editor)
  registerMarkdownShortcuts(editor, TRANSFORMERS)
  const { content } = useLexicalSync(
    editor,
    () => $convertToMarkdownString(TRANSFORMERS),
    (value) => $convertFromMarkdownString(value, TRANSFORMERS),
  )
  content.value = markdown.value
  syncRef(markdown, content, { immediate: false })
}
</script>

<template>
  <TextEditor
    :nodes="nodes"
    name="MarkdownEditor"
    class="fullHeight"
    @initialized="configure"
    @wheel.stop
    @contextmenu.stop
  />
</template>

<style scoped>
.fullHeight {
  height: 100%;
}
</style>

<style>
h1 {
  font-weight: 700;
  font-size: 16px;
  line-height: 1.75;
}

h2,
h3,
h4,
h5,
h6 {
  font-size: 14px;
  line-height: 2;
}

p + p {
  margin-bottom: 4px;
}
</style>
