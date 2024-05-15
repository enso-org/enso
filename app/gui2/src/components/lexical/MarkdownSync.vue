<script setup lang="ts">
import { useLexicalSync } from '@/components/lexical/sync'
import {
  $convertFromMarkdownString,
  $convertToMarkdownString,
  TRANSFORMERS,
} from '@lexical/markdown'
import { syncRef } from '@vueuse/core'
import { onMounted, onUnmounted } from 'vue'

const markdown = defineModel<string>({ required: true })

onMounted(() => {
  const { content, unregister } = useLexicalSync(
    () => $convertToMarkdownString(TRANSFORMERS),
    (value) => $convertFromMarkdownString(value, TRANSFORMERS),
  )
  syncRef(markdown, content)
  onUnmounted(unregister)
})
</script>

<!-- tslint:disable:vue/valid-template-root -->
<template />
