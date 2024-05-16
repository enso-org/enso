<script setup lang="ts">
import { useLexical, type LexicalPlugin } from '@/components/lexical'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
import SelectionFormattingToolbar from '@/components/lexical/SelectionFormattingToolbar.vue'
import { listPlugin } from '@/components/lexical/listPlugin'
import { useLexicalStringSync } from '@/components/lexical/sync'
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
import { ref, type ComponentInstance } from 'vue'

const markdown = defineModel<string>({ required: true })

const contentElement = ref<ComponentInstance<typeof LexicalContent>>()

const markdownPlugin: LexicalPlugin = {
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
    registerMarkdownShortcuts(editor, TRANSFORMERS)
  },
}

const markdownSyncPlugin: LexicalPlugin = {
  register: (editor) => {
    const { content } = useLexicalStringSync(
      editor,
      () => $convertToMarkdownString(TRANSFORMERS),
      (value) => $convertFromMarkdownString(value, TRANSFORMERS),
    )
    content.value = markdown.value
    syncRef(markdown, content, { immediate: false })
  },
}

const { editor } = useLexical(contentElement, 'MarkdownEditor', [
  listPlugin,
  markdownPlugin,
  markdownSyncPlugin,
])
</script>

<template>
  <div class="MarkdownEditor fullHeight">
    <LexicalContent ref="contentElement" class="fullHeight" @wheel.stop @contextmenu.stop />
    <SelectionFormattingToolbar v-if="contentElement" :editor="editor" />
  </div>
</template>

<style scoped>
.MarkdownEditor {
  position: relative;
}

.fullHeight {
  height: 100%;
}

.LexicalContent :deep(h1) {
  font-weight: 700;
  font-size: 16px;
  line-height: 1.75;
}

.LexicalContent :deep(h2, h3, h4, h5, h6) {
  font-size: 14px;
  line-height: 2;
}

.LexicalContent :deep(p + p) {
  margin-bottom: 4px;
}

.LexicalContent :deep(ol) {
  list-style-type: decimal;
  list-style-position: outside;
  padding-left: 1.6em;
}

.LexicalContent :deep(ul) {
  list-style-type: disc;
  list-style-position: outside;
  padding-left: 1.6em;
}

.LexicalContent :deep(strong) {
  font-weight: bold;
}

.LexicalContent :deep(.lexical-strikethrough) {
  text-decoration: line-through;
}
</style>
