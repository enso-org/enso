<script setup lang="ts">
import {
  provideDocumentationImageUrlTransformer,
  type UrlTransformer,
} from '@/components/MarkdownEditor/imageUrlTransformer'
import { HeaderLevel, ListType } from '@/util/codemirror'
import { Vec2 } from '@/util/data/vec2'
import { ComponentInstance, computed, defineAsyncComponent, ref, toRef } from 'vue'
import * as Y from 'yjs'

const props = defineProps<{
  content: Y.Text | string
  transformImageUrl?: UrlTransformer
  toolbarContainer: HTMLElement | undefined
}>()

const inner = ref<ComponentInstance<typeof LazyMarkdownEditor>>()

const LazyMarkdownEditor = defineAsyncComponent(
  () => import('@/components/MarkdownEditor/MarkdownEditorImpl.vue'),
)

provideDocumentationImageUrlTransformer(toRef(props, 'transformImageUrl'))

defineExpose({
  loaded: computed(() => inner.value != null),
  putText: (text: string) => {
    inner.value?.putText(text)
  },
  putTextAtCoord: (text: string, coords: Vec2) => {
    inner.value?.putTextAtCoords(text, coords)
  },
  toggleHeader: (level: HeaderLevel) => {
    inner.value?.toggleHeader(level)
  },
  toggleQuote: () => {
    inner.value?.toggleQuote()
  },
  toggleList: (type: ListType) => {
    inner.value?.toggleList(type)
  },
})
</script>

<template>
  <Suspense>
    <LazyMarkdownEditor ref="inner" v-bind="props" class="MarkdownEditor" />
  </Suspense>
</template>
