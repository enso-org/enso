<script setup lang="ts">
import { useSync } from '@/components/lexical/sync'
import { syncRef } from '@vueuse/core'
import { onMounted, onUnmounted } from 'vue'

const text = defineModel<string>({ required: true })

onMounted(() => {
  const { content, unregister } = useSync()
  syncRef(text, content)
  onUnmounted(unregister)
})
</script>

<template />
