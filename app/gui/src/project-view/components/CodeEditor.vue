<script setup lang="ts">
import { defineAsyncComponent } from 'vue'

const LazyCodeEditor = defineAsyncComponent(
  () => import('@/components/CodeEditor/CodeEditorImpl.vue'),
)
</script>

<template>
  <Suspense>
    <LazyCodeEditor />
  </Suspense>
</template>
