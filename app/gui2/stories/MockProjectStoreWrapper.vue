<script setup lang="ts">
import { onMounted, watch } from 'vue'

import diff from 'fast-diff'

import { useProjectStore } from '@/stores/project'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()

let ignoreNextUpdate = true

onMounted(() => projectStore.module?.doc.contents.insert(0, props.modelValue))

function applyEdits(
  module: NonNullable<typeof projectStore.module>,
  oldText: string,
  newText: string,
) {
  const contents = projectStore.module?.doc.contents
  if (!contents) return
  const edits = diff(oldText, newText)
  module.transact(() => {
    let i = 0
    for (const [type, string] of edits) {
      switch (type) {
        case -1: {
          contents.delete(i, string.length)
          i += string.length
          break
        }
        case 0: {
          i += string.length
          break
        }
        case 1: {
          contents.insert(i, string)
          break
        }
      }
    }
  })
}

watch(
  () => props.modelValue,
  (text, oldText) => {
    if (!projectStore.module) return
    ignoreNextUpdate = true
    applyEdits(projectStore.module, oldText, text)
  },
)

watch(
  () => projectStore.module?.doc.contents.toString() ?? '',
  (text, oldText) => {
    if (ignoreNextUpdate) {
      ignoreNextUpdate = false
      return
    }
    projectStore.module && applyEdits(projectStore.module, oldText, text)
    const newValue = projectStore.module?.doc.contents.toString()
    newValue != null && emit('update:modelValue', newValue)
  },
)
</script>

<template>
  <slot></slot>
</template>
