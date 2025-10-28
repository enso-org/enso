<script setup lang="ts">
import { type Node } from '$/providers/openedProjects/graph'
import PlainTextEditor from '@/components/PlainTextEditor.vue'
import { useFocusDelayed } from '@/composables/focus'
import { nodeMutableDocumentation } from '@/util/ast/node'
import { useYTextSync } from '@/util/codemirror'
import { syncRef } from '@vueuse/core'
import { computed, ref, type ComponentInstance } from 'vue'

const editing = defineModel<boolean>('editing', { required: true })
const props = defineProps<{ node: Node }>()

const textEditor = ref<ComponentInstance<typeof PlainTextEditor>>()
const textEditorContent = computed(() => textEditor.value?.contentElement)

const documentation = computed(() => nodeMutableDocumentation(props.node))

const { syncExt, connectSync } = useYTextSync(documentation, 'local:userAction:CommentEditor')

syncRef(editing, useFocusDelayed(textEditorContent).focused)
</script>
<template>
  <div
    v-if="documentation && (editing || documentation.toJSON().trimStart())"
    class="GraphNodeComment"
    @keydown.enter.capture.stop="editing = false"
  >
    <PlainTextEditor
      ref="textEditor"
      :extensions="syncExt"
      contentTestId="graph-node-comment-content"
      :editorReadyCallback="connectSync"
    />
  </div>
</template>

<style scoped>
:deep(.cm-content) {
  min-width: 22px;
  border-radius: var(--radius-default);
  background-color: var(--node-color-no-type);
  opacity: 0.8;
  color: var(--color-text-inversed);
  font-weight: 400;
  padding-left: 8px;
  padding-right: 8px;
}
</style>
