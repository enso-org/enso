<script setup lang="ts">
import { useLexical, type LexicalPlugin } from '@/components/lexical'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
import { useLexicalStringSync } from '@/components/lexical/sync'
import { registerPlainText } from '@lexical/plain-text'
import { ref, watch, type ComponentInstance } from 'vue'

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

useLexical(contentElement, 'PlainTextEditor', {}, [plainText, textSync])
</script>

<template>
  <LexicalContent ref="contentElement" />
</template>
