<script setup lang="ts">
import { useSync } from '@/components/lexical/sync'
import {
  $convertFromMarkdownString,
  $convertToMarkdownString,
  TRANSFORMERS,
} from '@lexical/markdown'
import { syncRef } from '@vueuse/core'
import { onMounted, onUnmounted } from 'vue'

const markdown = defineModel<string>({ required: true })

onMounted(() => {
  const { content, unregister } = useSync(
    () => $convertToMarkdownString(TRANSFORMERS),
    (value) => $convertFromMarkdownString(value, TRANSFORMERS),
  )
  syncRef(markdown, content)
  onUnmounted(unregister)
})
</script>

<template />
