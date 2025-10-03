<script setup lang="ts">
import type { Extension } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { computed, defineAsyncComponent, ref, type ComponentInstance } from 'vue'

const props = defineProps<{
  extensions?: Extension
  readonly?: boolean
  contentTestId?: string | undefined
  editorReadyCallback: (view: EditorView) => void
}>()

const impl = ref<ComponentInstance<typeof LazyPlainTextEditor>>()

const LazyPlainTextEditor = defineAsyncComponent(
  () => import('@/components/PlainTextEditor/PlainTextEditorImpl.vue'),
)

defineExpose({
  contentElement: computed(() => impl.value?.contentElement),
})
</script>

<template>
  <Suspense>
    <LazyPlainTextEditor ref="impl" v-bind="props" class="PlainTextEditor" />
  </Suspense>
</template>
