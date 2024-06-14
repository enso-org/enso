<script setup lang="ts">
import type { LexicalEditor } from 'lexical'
import { shallowRef, type DefineComponent } from 'vue'

const props = defineProps<{ editor: LexicalEditor }>()

const { decorators } = useDecorators(props.editor)

function useDecorators(editor: LexicalEditor) {
  const decorators = shallowRef<Record<string, DefineComponent>>(editor.getDecorators())
  editor.registerDecoratorListener((newValue) => {
    decorators.value = newValue as Record<string, DefineComponent>
  })
  return { decorators }
}
</script>

<template>
  <Teleport
    v-for="nodeKey in Object.keys(decorators)"
    :key="nodeKey"
    :to="editor.getElementByKey(nodeKey)"
  >
    <component :is="decorators[nodeKey]" />
  </Teleport>
</template>
