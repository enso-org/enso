<script setup lang="ts">
import { Extension } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { type ComponentInstance, computed, defineAsyncComponent, ref } from 'vue'

const props = defineProps<{
  extensions?: (view: EditorView) => Extension
  readonly?: boolean
  contentTestId?: string | undefined
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
