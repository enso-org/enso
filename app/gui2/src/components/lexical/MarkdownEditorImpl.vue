<script setup lang="ts">
import EditorRoot from '@/components/lexical/EditorRoot.vue'
import MarkdownSync from '@/components/lexical/MarkdownSync.vue'
import { useFocusDelayed } from '@/composables/focus'
import { CodeHighlightNode, CodeNode } from '@lexical/code'
import { AutoLinkNode, LinkNode } from '@lexical/link'
import { ListItemNode, ListNode } from '@lexical/list'
import { TRANSFORMERS } from '@lexical/markdown'
import { HeadingNode, QuoteNode } from '@lexical/rich-text'
import { TableCellNode, TableNode, TableRowNode } from '@lexical/table'
import { syncRef } from '@vueuse/core'
import {
  LexicalComposer,
  LexicalContentEditable,
  LexicalMarkdownShortcutPlugin,
  LexicalRichTextPlugin,
} from 'lexical-vue'
import { ref } from 'vue'

const text = defineModel<string>({ required: true })
const focused = defineModel<boolean>('focused', { default: false })

const root = ref<HTMLElement>()

const config = {
  editable: true,
  namespace: 'MarkdownEditor',
  theme: {},
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
}

syncRef(focused, useFocusDelayed(root).focused)
</script>

<template>
  <LexicalComposer :initial-config="config" @error="console.error($event)">
    <LexicalRichTextPlugin>
      <template #contentEditable>
        <LexicalContentEditable
          :spellcheck="false"
          class="lexicalContent"
          @wheel.stop
          @contextmenu.stop
        />
      </template>
    </LexicalRichTextPlugin>
    <MarkdownSync v-model="text" />
    <EditorRoot v-model="root" />
    <LexicalMarkdownShortcutPlugin :transformers="TRANSFORMERS" />
  </LexicalComposer>
</template>

<style scoped>
.lexicalContent {
  height: 100%;
  outline: none;
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
