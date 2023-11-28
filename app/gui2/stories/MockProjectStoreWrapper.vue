<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { useObserveYjs } from '@/util/crdt'
import diff from 'fast-diff'
import { computed, watchEffect } from 'vue'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
mod.doc.ydoc.emit('load', [])

function applyEdits(module: NonNullable<typeof projectStore.module>, newText: string) {
  const contents = projectStore.module?.doc.contents
  if (!contents) return
  const edits = diff(contents.toString(), newText)
  if (edits.length === 0) return

  module.transact(() => {
    let i = 0
    for (const [type, string] of edits) {
      switch (type) {
        case -1: {
          contents.delete(i, string.length)
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

watchEffect(() => projectStore.module && applyEdits(projectStore.module, props.modelValue))

const text = computed(() => projectStore.module?.doc.contents)

useObserveYjs(text, () => {
  if (text.value) {
    const newValue = text.value?.toString()
    if (newValue !== props.modelValue) {
      emit('update:modelValue', newValue)
    }
  }
})
</script>

<template>
  <slot></slot>
</template>
