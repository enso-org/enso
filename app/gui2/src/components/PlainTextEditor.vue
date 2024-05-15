<script setup lang="ts">
import TextEditor from '@/components/lexical/TextEditor.vue'
import { useLexicalSync } from '@/components/lexical/sync'
import { useBindKeyCommand } from '@/components/lexical/utils'
import { useFocusDelayed } from '@/composables/focus'
import { registerPlainText } from '@lexical/plain-text'
import { syncRef } from '@vueuse/core'
import { KEY_ENTER_COMMAND, type LexicalEditor } from 'lexical'
import { ref, type ComponentInstance } from 'vue'

const text = defineModel<string>({ required: true })
const focused = defineModel<boolean>('focused', { default: false })
const props = defineProps<{
  singleLine?: boolean
}>()

const editorComponent = ref<ComponentInstance<typeof LexicalEditor>>()

function configureEditor(editor: LexicalEditor) {
  registerPlainText(editor)
  syncRef(text, useLexicalSync(editor).content)
  syncRef(focused, useFocusDelayed(editorComponent).focused)
  if (props.singleLine) useBindKeyCommand(editor, KEY_ENTER_COMMAND, () => (focused.value = false))
}
</script>

<template>
  <TextEditor ref="editorComponent" name="PlainTextEditor" @initialized="configureEditor" />
</template>
