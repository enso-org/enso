<script setup lang="ts">
import { defineAsyncComponent } from 'vue'

const text = defineModel<string>({ required: true })

const LazyMarkdownEditor = defineAsyncComponent(
  () => import('@/components/lexical/MarkdownEditorImpl.vue'),
)
</script>

<template>
  <Suspense>
    <LazyMarkdownEditor v-model="text" />
  </Suspense>
</template>
