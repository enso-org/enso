<script setup lang="ts">
import type { UrlTransformer } from '@/components/MarkdownEditor/imageUrlTransformer'
import { defineAsyncComponent } from 'vue'

const text = defineModel<string>({ required: true })
const props = defineProps<{
  transformImageUrl?: UrlTransformer
  toolbarContainer: HTMLElement | undefined
}>()

const LazyMarkdownEditor = defineAsyncComponent(
  () => import('@/components/MarkdownEditor/MarkdownEditorImpl.vue'),
)
</script>

<template>
  <Suspense>
    <LazyMarkdownEditor v-model="text" v-bind="props" />
  </Suspense>
</template>
