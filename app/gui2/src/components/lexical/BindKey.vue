<script setup lang="ts">
import { COMMAND_PRIORITY_NORMAL, type LexicalCommand } from 'lexical'
import { useLexicalComposer } from 'lexical-vue'
import { onMounted, onUnmounted } from 'vue'

const props = defineProps<{
  commandKey: LexicalCommand<KeyboardEvent | null>
}>()
const emit = defineEmits<{
  pressed: []
}>()

const editor = useLexicalComposer()

onMounted(() => {
  const unregister = editor.registerCommand(
    props.commandKey,
    (event) => {
      if (event) {
        event.preventDefault()
        event.stopImmediatePropagation()
      }
      emit('pressed')
      return true
    },
    COMMAND_PRIORITY_NORMAL,
  )
  onUnmounted(unregister)
})
</script>

<!-- tslint:disable:vue/valid-template-root -->
<template />
