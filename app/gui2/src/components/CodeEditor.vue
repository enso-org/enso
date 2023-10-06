<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { useWindowEvent } from '@/util/events'
import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { minimalSetup } from 'codemirror'
import { ref, watchPostEffect } from 'vue'
// y-codemirror.next does not provide type information. See https://github.com/yjs/y-codemirror.next/issues/27
// @ts-ignore
import { yCollab } from 'y-codemirror.next'

const projectStore = useProjectStore()

// == Keyboard shortcut to toggle the CodeEditor ==

const shown = ref(false)
const rootElement = ref<HTMLElement>()

useWindowEvent('keydown', (e) => {
  const graphEditorInFocus = document.activeElement === document.body
  const codeEditorInFocus = rootElement.value?.contains(document.activeElement)
  const validFocus = graphEditorInFocus || codeEditorInFocus
  const targetKeyPressed = e.key == `\``
  if (validFocus && targetKeyPressed) {
    e.preventDefault()
    shown.value = !shown.value
  }
})

// == CodeMirror editor setup  ==

const codeMirrorEl = ref(null)
const editorView = ref<EditorView>()
watchPostEffect((onCleanup) => {
  const module = projectStore.module
  if (!module || !codeMirrorEl.value) return
  const yText = module.doc.contents
  const undoManager = module.undoManager
  const awareness = projectStore.awareness
  const view = new EditorView({
    parent: codeMirrorEl.value,
    state: EditorState.create({
      doc: yText.toString(),
      extensions: [minimalSetup, yCollab(yText, awareness, { undoManager })],
    }),
  })
  onCleanup(() => view.destroy())
  editorView.value = view
})
</script>

<template>
  <div
    v-show="shown"
    ref="rootElement"
    class="CodeEditor"
    @keydown.enter.stop
    @wheel.stop.passive
    @pointerdown.stop
  >
    <div ref="codeMirrorEl" class="codemirror-container"></div>
  </div>
</template>

<style>
.CodeEditor {
  position: absolute;
  bottom: 0;
  left: 0;
  width: 50%;
  height: 30%;
}
.codemirror-container {
  width: 100%;
  height: 100%;
  background-color: rgba(0, 0, 0, 0.5);
  backdrop-filter: blur(4px);
  border-top-right-radius: 10px;
  opacity: 1;
  color: white;
}
.cm-editor {
  width: 100%;
  height: 100%;
}
.cm-gutters {
  display: none !important;
}
</style>
