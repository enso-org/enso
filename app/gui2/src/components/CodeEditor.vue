<script setup lang="ts">
import { ref, watchEffect } from 'vue'
import { useWindowEvent } from '@/util/events'

const emit = defineEmits<{
  codeUpdate: [code: string]
}>()

const content = ref('main = \n    2 + 2')
const shown = ref(false)

watchEffect(() => {
  emit('codeUpdate', content.value)
})

const textArea = ref<InstanceType<typeof HTMLTextAreaElement> | null>(null)

useWindowEvent('keydown', (e) => {
  const graphEditorInFocus = document.activeElement === document.body
  const codeEditorInFocus =
    document.activeElement === document.querySelector('.CodeEditor > textarea')
  const validFocus = graphEditorInFocus || codeEditorInFocus
  const targetKeyPressed = e.key == `\``
  if (validFocus && targetKeyPressed) {
    e.preventDefault()
    shown.value = !shown.value
  }
})

watchEffect(
  () => {
    if (shown.value) {
      textArea.value?.focus()
    } else {
      textArea.value?.blur()
    }
  },
  { flush: 'post' },
)
</script>

<template>
  <div v-if="shown" class="CodeEditor" @keydown.enter.stop>
    <textarea v-model="content" ref="textArea"></textarea>
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
