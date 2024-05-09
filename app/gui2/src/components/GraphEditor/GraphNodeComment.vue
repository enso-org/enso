<script setup lang="ts">
import type { EditorView as EditorViewType } from '@/components/CodeEditor/codemirror'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { assertDefined } from 'shared/util/assert'
import { textChangeToEdits } from 'shared/util/data/text'
import { computed, onMounted, onUnmounted, ref, watchEffect } from 'vue'

const { minimalSetup, EditorState, EditorView, textEditToChangeSpec } = await import(
  '@/components/CodeEditor/codemirror'
)

const props = defineProps<{ modelValue: string; editing: boolean }>()
const emit = defineEmits<{
  'update:modelValue': [modelValue: string | undefined]
  'update:editing': [boolean]
}>()

const text = computed({
  get: () => rawTextToCooked(props.modelValue),
  set: (value) => emit('update:modelValue', cookedTextToRaw(value)),
})

const commentRoot = ref<HTMLElement>()
const editor = ref<EditorViewType>()

const interactions = injectInteractionHandler()
const editInteraction = {
  cancel: () => finishEdit(),
  end: () => finishEdit(),
  click: (e: Event) => {
    if (e.target instanceof Element && !commentRoot.value?.contains(e.target)) finishEdit()
    return false
  },
}
interactions.setWhen(() => props.editing, editInteraction)
onUnmounted(() => interactions.end(editInteraction))

function startEdit() {
  if (!props.editing) emit('update:editing', true)
}

function finishEdit() {
  if (props.editing) {
    if (editor.value) {
      const viewText = editor.value.state.doc.toString()
      if (viewText !== text.value) text.value = viewText
    }
    emit('update:editing', false)
  }
}

function insertTextAtCursor(insert: string) {
  if (!editor.value) return
  const range = editor.value.state.selection.ranges[0] ?? { from: 0, to: 0 }
  editor.value.dispatch({
    changes: {
      from: range.from,
      to: range.to,
      insert,
    },
    selection: { anchor: range.from + insert.length },
  })
}

function handleEnter(event: KeyboardEvent) {
  if (event.shiftKey) insertTextAtCursor('\n')
  else finishEdit()
}

watchEffect(() => {
  if (!editor.value) return
  const viewText = editor.value.state.doc.toString()
  editor.value.dispatch({
    changes: textChangeToEdits(viewText, text.value).map(textEditToChangeSpec),
  })
})

watchEffect(() => {
  if (!editor.value) return
  if (props.editing) editor.value.focus()
  else editor.value.contentDOM.blur()
})

onMounted(() => {
  assertDefined(commentRoot.value)
  const editorView = new EditorView({ parent: commentRoot.value })
  editorView.setState(EditorState.create({ extensions: [minimalSetup, EditorView.lineWrapping] }))
  editor.value = editorView
})
</script>
<script lang="ts">
/** Interpret a comment from source-code format to display format.
 *
 *  Hard-wrapped lines are combined similarly to how whitespace is interpreted in Markdown:
 *  - A single linebreak is treated as a space.
 *  - A sequence of linebreaks is treated as a paragraph-break.
 */
export function rawTextToCooked(raw: string) {
  return raw.replaceAll(/(?<!\n)\n(?!\n)/g, ' ').replaceAll(/\n(\n+)/g, '$1')
}

/** Invert the transformation applied by @{rawTextToCooked}. */
export function cookedTextToRaw(cooked: string) {
  return cooked.replaceAll('\n', '\n\n')
}
</script>

<template>
  <div
    ref="commentRoot"
    class="GraphNodeComment"
    @keydown.enter.capture.stop.prevent="handleEnter"
    @keydown.space.stop
    @keydown.delete.stop
    @wheel.stop.passive
    @focusout="finishEdit"
    @click.stop="startEdit"
    @contextmenu.stop
  ></div>
</template>

<style scoped>
.GraphNodeComment {
  width: max(100% - 60px, 800px);
}

:deep(.cm-editor) {
  position: absolute;
  bottom: 100%;
  display: inline-block;
  padding: 0 0 2px;
  border-radius: var(--radius-default);
  background-color: var(--node-color-no-type);
  outline: 0;
}

:deep(.cm-content) {
  font-family: var(--font-sans);
  font-weight: 400;
  color: var(--color-text-inversed);
  line-height: 18px;
}

:deep(.cm-content),
:deep(.cm-line) {
  padding: 0;
}
:deep(.cm-scroller) {
  width: 100%;
}
:deep(.cm-scroller) {
  overflow-x: clip;
  /* Horizontal padding is in the CodeMirror element so that it has room to draw its cursor. */
  padding: 0 8px;
}

:deep(.cm-editor),
:deep(.cm-line) {
  width: fit-content;
}
</style>
