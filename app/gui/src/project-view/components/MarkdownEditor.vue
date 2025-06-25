<script setup lang="ts">
import { Extension } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { defineAsyncComponent, Ref } from 'vue'

// Toolbar is singled out, because missing booleans coerce to false instead of undefined
// and toolbar has default `true` in inner component
const { toolbar = true, ...props } = defineProps<{
  toolbar?: boolean
  readonly?: boolean
  extensions?: (view: EditorView, focused: Ref<boolean>) => Extension
  contentTestId?: string
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
