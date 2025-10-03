<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { selectOnMouseFocus, useCodeMirror, useStringSync } from '@/util/codemirror'
import { useTemplateRef, watch } from 'vue'

const model = defineModel<string>({ required: true })
const { active, editing } = defineProps<{ active: boolean; editing: boolean }>()

const editorRoot = useTemplateRef('editorRoot')

const { syncExt, getText, setText } = useStringSync()
const { editorView } = useCodeMirror(editorRoot, {
  extensions: [syncExt, selectOnMouseFocus],
  readonly: false,
  lineMode: 'single',
})

watch(model, (text) => setText(editorView, text), { immediate: true })
function onEditorBlur() {
  model.value = getText(editorView)
}

function accept() {
  editorView.contentDOM.blur()
}

function focusEditor() {
  editorView.dispatch({ selection: { anchor: 0, head: editorView.state.doc.length } })
  editorView.focus()
}

watch(editorRoot, (editorRoot) => {
  if (editorRoot) focusEditor()
})
</script>

<template>
  <div class="NavBreadcrumb" :class="{ inactive: !active }">
    <CodeMirrorRoot
      v-if="editing"
      ref="editorRoot"
      @focusout="onEditorBlur"
      @keydown.enter.stop="accept"
      @keydown.tab.stop="accept"
    />
    <template v-else>{{ model }}</template>
  </div>
</template>

<style scoped>
.NavBreadcrumb {
  padding-bottom: 2px;
  user-select: none;
  border-radius: var(--radius-full);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  flex-shrink: 1;
  min-width: 0;
}

.CodeMirrorRoot {
  pointer-events: auto;
}

.inactive {
  opacity: 0.4;
}
</style>
