<script setup lang="ts">
import { defineAsyncComponent } from 'vue'
import * as Y from 'yjs'

const {
  content,
  toolbar = true,
  contentTestId,
  scrollerTestId,
} = defineProps<{
  content: Y.Text | string
  toolbar?: boolean
  contentTestId?: string | undefined
  scrollerTestId?: string | undefined
}>()
defineOptions({
  inheritAttrs: false,
})

const LazyMarkdownEditor = defineAsyncComponent(
  () => import('@/components/MarkdownEditor/MarkdownEditorImpl.vue'),
)
</script>

<template>
  <Suspense>
    <LazyMarkdownEditor
      v-bind="$attrs"
      class="flex-1"
      :content="content"
      :toolbar="toolbar"
      :contentTestId="contentTestId"
      :scrollerTestId="scrollerTestId"
    >
      <template #belowToolbar>
        <slot name="belowToolbar" />
      </template>
    </LazyMarkdownEditor>
  </Suspense>
</template>
