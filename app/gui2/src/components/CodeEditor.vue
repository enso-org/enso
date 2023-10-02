<script setup lang="ts">
import { useGraphStore } from '@/stores/graph'
import { useWindowEvent } from '@/util/events'
import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { basicSetup } from 'codemirror'
import { onMounted, ref, watch } from 'vue'
// y-codemirror.next does not provide type information. See https://github.com/yjs/y-codemirror.next/issues/27
// @ts-ignore
import { yCollab } from 'y-codemirror.next'

let graphStore = useGraphStore()

// == Keyboard shortcut to toggle the CodeEditor ==

const shown = ref(false)
const rootElement = ref<HTMLElement>()

useWindowEvent('keydown', (e) => {
  console.log('keydown', e)
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
onMounted(() => {
  watch(
    () => graphStore.proj.module,
    (module) => {
      // Note that this only works while we do not switch documents at runtime and have a single module.
      const yText = module?.contents
      if (!yText) {
        console.error('No module content available')
        return
      }
      const undoManager = graphStore.proj.undoManager
      const awareness = graphStore.proj.awareness
      const state = EditorState.create({
        doc: yText.toString(),
        extensions: [basicSetup, yCollab(yText, awareness, { undoManager })],
      })
      if (!codeMirrorEl.value) {
        console.error('Parent element for CodeMirror not found')
        return
      }
      editorView.value = new EditorView({
        parent: codeMirrorEl.value,
        state,
      })
    },
  )
})
</script>

<template>
  <div v-show="shown" ref="rootElement" class="CodeEditor" @keydown.enter.stop>
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
  background-color: rgba(255, 255, 255, 0.3);
}
.cm-editor {
  width: 100%;
  height: 100%;
}
.cm-gutters {
  display: none !important;
}
</style>
