<script setup lang="ts">
import TextEditor from '@/components/lexical/TextEditor.vue'
import { useLexicalSync } from '@/components/lexical/sync'
import { registerPlainText } from '@lexical/plain-text'
import { syncRef } from '@vueuse/core'
import { type LexicalEditor } from 'lexical'

const text = defineModel<string>({ required: true })

function configure(editor: LexicalEditor) {
  registerPlainText(editor)
  const { content } = useLexicalSync(editor)
  content.value = text.value
  syncRef(text, content, { immediate: false })
}
</script>

<template>
  <TextEditor name="PlainTextEditor" @initialized="configure" />
</template>
