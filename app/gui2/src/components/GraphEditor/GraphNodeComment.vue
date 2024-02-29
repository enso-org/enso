<script setup lang="ts">
import type { EditorView as EditorViewType } from '@/components/CodeEditor/codemirror'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { defineKeybinds } from '@/util/shortcuts'
import * as random from 'lib0/random'
import { textChangeToEdits } from 'shared/util/data/text'
import { computed, ref, watch, watchEffect } from 'vue'

const { minimalSetup, EditorState, EditorView, textEditToChangeSpec } = await import(
  '@/components/CodeEditor/codemirror'
)

const props = defineProps<{ modelValue: string; editing: boolean }>()
const emit = defineEmits<{
  'update:modelValue': [modelValue: string | undefined]
  'update:editing': [boolean]
}>()

const paragraphs = computed(() => props.modelValue.split('\n\n'))

const contentElement = ref<HTMLElement>()
const editor = ref<EditorViewType>()

const interaction = injectInteractionHandler()
interaction.setWhen(() => editor.value != null, {
  cancel() {
    finishEdit()
  },
  click(e: Event) {
    if (e.target instanceof Element && !contentElement.value?.contains(e.target)) finishEdit()
    return false
  },
})

const handleClick = defineKeybinds(`comment-${random.uint53()}`, {
  startEdit: ['Mod+PointerMain'],
}).handler({ startEdit })

function startEdit() {
  const editorView = new EditorView()
  editorView.setState(EditorState.create({ extensions: [minimalSetup] }))
  contentElement.value!.prepend(editorView.dom)
  editor.value = editorView
  if (!props.editing) emit('update:editing', true)
  setTimeout(() => editorView.focus())
}

function finishEdit() {
  if (!editor.value) return
  if (editor.value.state.doc.toString() !== props.modelValue)
    emit('update:modelValue', editor.value.state.doc.toString())
  editor.value.dom.remove()
  editor.value = undefined
  if (props.editing) emit('update:editing', false)
}

watchEffect(() => {
  const text = props.modelValue
  if (!editor.value) return
  const viewText = editor.value.state.doc.toString()
  editor.value.dispatch({
    changes: textChangeToEdits(viewText, text).map(textEditToChangeSpec),
  })
})

watchEffect(() => {
  if (contentElement.value && props.editing && !editor.value) startEdit()
})
</script>

<template>
  <div
    class="GraphNodeComment"
    @keydown.enter.stop
    @keydown.backspace.stop
    @keydown.space.stop
    @keydown.delete.stop
    @wheel.stop.passive
    @blur="finishEdit"
    @pointerdown.stop="handleClick"
    @pointerup.stop
    @click.stop
    @contextmenu.stop
  >
    <div ref="contentElement" class="content">
      <template v-if="!editor">
        <p v-for="(paragraph, i) in paragraphs" :key="i" v-text="paragraph" />
      </template>
    </div>
  </div>
</template>

<style scoped>
.GraphNodeComment {
  width: max(100% - 60px, 800px);
}

.content {
  position: absolute;
  bottom: 100%;
  display: inline-block;
  padding: 0 8px 2px;
  font-weight: 400;
  border-radius: var(--radius-default);
  color: var(--color-text-inversed);
  line-height: 18px;
  background-color: var(--node-color-no-type);
  max-width: 100%;
  overflow-x: auto;
}
</style>
