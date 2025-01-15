<script setup lang="ts">
import { type ComponentInstance, computed, defineAsyncComponent, ref } from 'vue'
import * as Y from 'yjs'

const props = defineProps<{ content: Y.Text | string }>()

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
