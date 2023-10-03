<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />
import diff from 'fast-diff'
import { ref, watch } from 'vue'

import { useProjectStore } from '@/stores/project'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const text = ref('')

const projectStore = useProjectStore()

watch(
  () => props.modelValue,
  (value, oldValue) => {
    text.value = value
    const module = projectStore.module
    if (module) {
      const edits = diff(oldValue, value)
      module.transact(() => {
        const text = module.doc.contents
        let i = 0
        for (const [type, string] of edits) {
          switch (type) {
            case -1: {
              text.delete(i, string.length)
              i += string.length
              break
            }
            case 0: {
              i += string.length
              break
            }
            case 1: {
              text.insert(i, string)
              break
            }
          }
        }
      })
    }
  },
)

watch(
  () => projectStore.module?.doc.contents.toString(),
  (contents) => {
    if (contents && contents !== text.value) {
      text.value = contents
      emit('update:modelValue', contents)
    }
  },
)
</script>

<template>
  <slot></slot>
</template>
