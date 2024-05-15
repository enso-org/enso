<script setup lang="ts">
import type { KlassConstructor, LexicalEditor, LexicalNode, LexicalNodeReplacement } from 'lexical'
import { createEditor } from 'lexical'
import { onMounted, ref } from 'vue'

const props = defineProps<{
  name: string
  nodes?: readonly (KlassConstructor<typeof LexicalNode> | LexicalNodeReplacement)[]
}>()
const emit = defineEmits<{
  initialized: [LexicalEditor]
}>()

const lexicalElement = ref<HTMLElement>()

const editor = createEditor({
  editable: true,
  namespace: props.name,
  theme: {},
  nodes: props.nodes ?? [],
  onError: console.error,
})

onMounted(() => {
  if (!lexicalElement.value) return
  editor.setRootElement(lexicalElement.value)
  emit('initialized', editor)
})
</script>

<template>
  <div ref="lexicalElement" class="lexical" spellcheck="false" contenteditable="true" />
</template>

<style scoped>
.lexical {
  outline-style: none;
}
</style>
