<script setup lang="ts">
import { useLexical, type LexicalPlugin } from '@/components/lexical'
import LexicalContent from '@/components/lexical/LexicalContent.vue'
import { useLexicalSync } from '@/components/lexical/sync'
import { registerPlainText } from '@lexical/plain-text'
import { syncRef } from '@vueuse/core'
import { ref, type ComponentInstance } from 'vue'

const text = defineModel<string>({ required: true })

const contentElement = ref<ComponentInstance<typeof LexicalContent>>()

const plainText: LexicalPlugin = {
  register: registerPlainText,
}

const textSync: LexicalPlugin = {
  register: (editor) => {
    const { content } = useLexicalSync(editor)
    content.value = text.value
    syncRef(text, content, { immediate: false })
  },
}

useLexical(contentElement, 'PlainTextEditor', [plainText, textSync])
</script>

<template>
  <LexicalContent ref="contentElement" />
</template>
