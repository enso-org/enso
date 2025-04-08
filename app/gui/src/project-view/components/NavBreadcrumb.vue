<script setup lang="ts">
import CodeMirrorInlineRoot from '@/components/CodeMirrorInlineRoot.vue'
import { selectOnMouseFocus, useCodeMirror, useStringSync } from '@/util/codemirror'
import { useTemplateRef, watch, type ComponentInstance } from 'vue'

const model = defineModel<string>({ required: true })
const { active, editing } = defineProps<{ active: boolean; editing: boolean }>()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorInlineRoot>>('editorRoot')

const { syncExt, connectSync } = useStringSync()
const { editorView } = useCodeMirror(editorRoot, {
  content: model.value,
  extensions: [syncExt, selectOnMouseFocus],
  readonly: false,
  singleLine: true,
})

const { getText, setText } = connectSync(editorView)
watch(model, (text) => setText(text))
function onEditorBlur() {
  model.value = getText()
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
    <CodeMirrorInlineRoot
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
}

.CodeMirrorInlineRoot {
  pointer-events: auto;
  cursor: text;
}

.inactive {
  opacity: 0.4;
}
</style>
