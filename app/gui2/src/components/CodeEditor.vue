<script setup lang="ts">
import { ref, watchEffect } from 'vue'
import { useWindowEvent } from '@/util/events'

const emit = defineEmits<{
  codeUpdate: [code: string]
}>()

const content = ref('')
const shown = ref(false)

watchEffect(() => {
  emit('codeUpdate', content.value)
})

const textArea = ref<HTMLElement>()
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

watchEffect(
  () => {
    /// If the code editor is shown, focus the text area to allow typing.
    if (shown.value) {
      textArea.value?.focus()
    }
  },
  { flush: 'post' },
)
</script>

<template>
  <div v-if="shown" ref="rootElement" class="CodeEditor" @keydown.enter.stop>
    <textarea ref="textArea" v-model="content"></textarea>
  </div>
</template>

<style scoped>
.CodeEditor {
  position: absolute;
  bottom: 0;
  left: 0;
}

.CodeEditor > textarea {
  background-color: rgba(1, 1, 1, 0.1);
  border: none;
  resize: none;
  width: 500px;
  height: 500px;
}

.CodeEditor > textarea:focus {
  outline: none !important;
}
</style>
