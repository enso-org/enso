<script setup lang="ts">
import PlainTextEditor from '@/components/PlainTextEditor.vue'
import { useFocusDelayed } from '@/composables/focus'
import { type Node } from '@/stores/graph'
import { nodeMutableDocumentation } from '@/util/ast/node'
import { syncRef } from '@vueuse/core'
import { computed, ref, type ComponentInstance } from 'vue'

const editing = defineModel<boolean>('editing', { required: true })
const props = defineProps<{ node: Node }>()

const textEditor = ref<ComponentInstance<typeof PlainTextEditor>>()
const textEditorContent = computed(() => textEditor.value?.contentElement)

const documentation = computed(() => nodeMutableDocumentation(props.node))

syncRef(editing, useFocusDelayed(textEditorContent).focused)
</script>
<template>
  <div
    v-if="documentation && (editing || documentation.toJSON().trimStart())"
    class="GraphNodeComment"
    @keydown.enter.capture.stop="editing = false"
  >
    <PlainTextEditor ref="textEditor" :content="documentation" />
  </div>
</template>

<style scoped>
:deep(.cm-content) {
  display: inline-block;
  min-width: 22px;
  border-radius: var(--radius-default);
  background-color: var(--node-color-no-type);
  opacity: 0.8;
  color: var(--color-text-inversed);
  font-weight: 400;
}

:deep(.cm-line) {
  padding: 0 8px 0 8px;
}
</style>
