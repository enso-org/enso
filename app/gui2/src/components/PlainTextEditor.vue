<script setup lang="ts">
import FloatingSelectionMenu from '@/components/MarkdownEditor/FloatingSelectionMenu.vue'
import { autoLinkPlugin } from '@/components/MarkdownEditor/LinkPlugin'
import LinkToolbar from '@/components/MarkdownEditor/LinkToolbar.vue'
import { lexicalRichTextTheme } from '@/components/MarkdownEditor/formatting'
import { useLinkNode } from '@/components/MarkdownEditor/link'
import { useLexical, type LexicalPlugin } from '@/components/lexical'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
import { useLexicalStringSync } from '@/components/lexical/sync'
import { registerPlainText } from '@lexical/plain-text'
import { ref, useCssModule, watch, type ComponentInstance } from 'vue'

const text = defineModel<string>({ required: true })

const contentElement = ref<ComponentInstance<typeof LexicalContent>>()

const plainText: LexicalPlugin = {
  register: registerPlainText,
}

const textSync: LexicalPlugin = {
  register: (editor) => {
    const { content } = useLexicalStringSync(editor)
    watch(text, (newContent) => content.set(newContent), { immediate: true })
    watch(content.state, (newContent) => (text.value = newContent))
  },
}

const theme = lexicalRichTextTheme(useCssModule('lexicalTheme'))
const { editor } = useLexical(contentElement, 'PlainTextEditor', theme, [
  autoLinkPlugin,
  plainText,
  textSync,
])
const { urlUnderCursor } = useLinkNode(editor)
</script>

<template>
  <LexicalContent ref="contentElement" />
  <FloatingSelectionMenu :selectionElement="contentElement">
    <LinkToolbar v-if="urlUnderCursor" :url="urlUnderCursor" />
  </FloatingSelectionMenu>
</template>

<style module="lexicalTheme">
.link {
  color: #ddf;
  &:hover {
    text-decoration: underline;
  }
}
</style>
