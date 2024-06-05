<script setup lang="ts">
import FloatingSelectionMenu from '@/components/MarkdownEditor/FloatingSelectionMenu.vue'
import FormattingToolbar from '@/components/MarkdownEditor/FormattingToolbar.vue'
import SelectionFormattingToolbar from '@/components/MarkdownEditor/SelectionFormattingToolbar.vue'
import { useFormatting } from '@/components/MarkdownEditor/formatting'
import { listPlugin } from '@/components/MarkdownEditor/listPlugin'
import { lexicalTheme, useLexical, type LexicalPlugin } from '@/components/lexical'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
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
import { shallowRef, useCssModule, watch, type ComponentInstance } from 'vue'

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
    watch(markdown, (newContent) => content.set(newContent), { immediate: true })
    watch(content.state, (newContent) => (markdown.value = newContent))
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
:deep(.DropdownMenuContent .MenuButton) {
  justify-content: unset;
}
</style>

<style module="lexicalTheme" src="@/components/MarkdownEditor/theme.css" />
