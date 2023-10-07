<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { usePointer } from '@/util/events'
import { useLocalStorage } from '@vueuse/core'
import { computed, onMounted, ref, watchEffect } from 'vue'

// Use dynamic imports to aid code splitting. The codemirror dependency is quite large.
const { minimalSetup, EditorState, EditorView, yCollab } = await import('@/util/codemirror')

const projectStore = useProjectStore()
const rootElement = ref<HTMLElement>()

// == CodeMirror editor setup  ==

const editorView = new EditorView()
watchEffect(() => {
  const module = projectStore.module
  if (!module) return
  const yText = module.doc.contents
  const undoManager = module.undoManager
  const awareness = projectStore.awareness
  editorView.setState(
    EditorState.create({
      doc: yText.toString(),
      extensions: [minimalSetup, yCollab(yText, awareness, { undoManager })],
    }),
  )
})

onMounted(() => {
  editorView.focus()
  rootElement.value?.prepend(editorView.dom)
})

const editorSize = useLocalStorage<{ width: number | null; height: number | null }>(
  'code-editor-size',
  { width: null, height: null },
)

let initSize = { width: 0, height: 0 }
const resize = usePointer((pos, _, type) => {
  if (rootElement.value == null) return
  if (type == 'start') initSize = rootElement.value.getBoundingClientRect()
  editorSize.value.width = initSize.width + pos.relative.x
  editorSize.value.height = initSize.height - pos.relative.y
})

function resetSize() {
  editorSize.value.width = null
  editorSize.value.height = null
}

const editorStyle = computed(() => {
  return {
    width: editorSize.value.width ? `${editorSize.value.width}px` : '50%',
    height: editorSize.value.height ? `${editorSize.value.height}px` : '30%',
  }
})
</script>

<template>
  <div
    ref="rootElement"
    class="CodeEditor"
    :style="editorStyle"
    @keydown.enter.stop
    @wheel.stop.passive
    @pointerdown.stop
  >
    <div class="resize-handle" v-on="resize.events" @dblclick="resetSize">
      <svg viewBox="0 0 16 16">
        <circle cx="2" cy="2" r="1.5" />
        <circle cx="8" cy="2" r="1.5" />
        <circle cx="8" cy="8" r="1.5" />
        <circle cx="14" cy="2" r="1.5" />
        <circle cx="14" cy="8" r="1.5" />
        <circle cx="14" cy="14" r="1.5" />
      </svg>
    </div>
  </div>
</template>

<style>
.CodeEditor {
  position: absolute;
  bottom: 5px;
  left: 5px;
  width: 50%;
  height: 30%;
  max-width: calc(100% - 10px);
  max-height: calc(100% - 10px);
  backdrop-filter: blur(16px);

  &.v-enter-active,
  &.v-leave-active {
    transition:
      transform 0.2s ease,
      opacity 0.2s ease;
  }

  &.v-enter-from,
  &.v-leave-to {
    transform: scale(95%);
    opacity: 0;
  }
}

.resize-handle {
  position: absolute;
  top: -3px;
  right: -3px;
  width: 20px;
  height: 20px;
  padding: 5px;
  /* cursor: nesw-resize; */

  svg {
    fill: black;
    width: 100%;
    height: 100%;
    opacity: 0.1;
    transition: opacity 0.1s ease-in-out;
  }

  &:hover svg {
    opacity: 0.9;
  }
}

.CodeEditor :is(.cm-editor) {
  color: white;
  width: 100%;
  height: 100%;
  background-color: rgba(255, 255, 255, 0.35);
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.4);
  border-radius: 5px;
  opacity: 1;
  color: black;
  text-shadow: 0 0 2px rgba(255, 255, 255, 0.4);
  font-size: 12px;
  outline: 1px solid transparent;
  transition: outline 0.1s ease-in-out;
}
.CodeEditor :is(.cm-focused) {
  outline: 1px solid rgba(0, 0, 0, 0.5);
}
</style>
