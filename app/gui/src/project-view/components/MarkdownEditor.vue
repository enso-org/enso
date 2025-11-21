<script setup lang="ts">
import type { Extension } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { defineAsyncComponent } from 'vue'

// Toolbar is singled out, because missing booleans coerce to false instead of undefined
// and toolbar has default `true` in inner component
const { toolbar = true, ...props } = defineProps<{
  toolbar?: boolean
  readonly?: boolean
  extensions?: Extension
  contentTestId?: string
  scrollerTestId?: string | undefined
  editorReadyCallback?: ((view: EditorView) => void) | undefined
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
    <LazyMarkdownEditor v-bind="{ ...$attrs, ...props }" :toolbar="toolbar" class="flex-1">
      <template #belowToolbar>
        <slot name="belowToolbar" />
      </template>
    </LazyMarkdownEditor>
  </Suspense>
</template>
