<script setup lang="ts">
import { lexicalTheme, useLexical, type LexicalPlugin } from '@/components/lexical'
import FloatingSelectionMenu from '@/components/lexical/FloatingSelectionMenu.vue'
import FormattingToolbar from '@/components/lexical/FormattingToolbar.vue'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
import SelectionFormattingToolbar from '@/components/lexical/SelectionFormattingToolbar.vue'
import { useFormatting } from '@/components/lexical/formatting'
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
import { shallowRef, useCssModule, type ComponentInstance } from 'vue'

const markdown = defineModel<string>({ required: true })

const contentElement = shallowRef<ComponentInstance<typeof LexicalContent>>()

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

const theme = lexicalTheme(useCssModule('lexicalTheme'))
const { editor } = useLexical(contentElement, 'MarkdownEditor', theme, [
  listPlugin,
  markdownPlugin,
  markdownSyncPlugin,
])
const formatting = useFormatting(editor)
</script>

<template>
  <div class="MarkdownEditor fullHeight">
    <FormattingToolbar :formatting="formatting" @pointerdown.prevent />
    <LexicalContent ref="contentElement" class="fullHeight" @wheel.stop @contextmenu.stop />
    <FloatingSelectionMenu :selectionElement="contentElement">
      <SelectionFormattingToolbar :formatting="formatting" />
    </FloatingSelectionMenu>
  </div>
</template>

<style scoped>
.fullHeight {
  height: 100%;
}

:deep(.toggledOn) {
  color: black;
  opacity: 0.6;
}
:deep(.toggledOff) {
  color: black;
  opacity: 0.3;
}
:deep(.DropdownMenuButton) {
  color: inherit;
  opacity: inherit;
}
</style>

<style module="lexicalTheme" src="@/components/lexical/theme.css" />
