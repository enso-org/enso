<script setup lang="ts">
import FloatingSelectionMenu from '@/components/FloatingSelectionMenu.vue'
import { lexicalTheme, useLexical, type LexicalPlugin } from '@/components/lexical'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
import { autoLinkPlugin, useLinkNode } from '@/components/lexical/LinkPlugin'
import LinkToolbar from '@/components/lexical/LinkToolbar.vue'
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
    watch(content.editedContent, (newContent) => (text.value = newContent))
  },
}

const theme = lexicalTheme(useCssModule('lexicalTheme'))
const { editor } = useLexical(contentElement, 'PlainTextEditor', theme, [
  autoLinkPlugin,
  plainText,
  textSync,
])
const { urlUnderCursor } = useLinkNode(editor)

defineExpose({ contentElement })
</script>

<template>
  <LexicalContent ref="contentElement" v-bind="$attrs" />
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
