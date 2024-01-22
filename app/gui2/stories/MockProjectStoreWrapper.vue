<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { useObserveYjs } from '@/util/crdt'
import { computed, watchEffect } from 'vue'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
projectStore.setObservedFileName('Main.enso')
mod.doc.ydoc.emit('load', [])

function applyEdits(module: NonNullable<typeof projectStore.module>, newText: string) {
  module.transact(() => {
    projectStore.module?.doc.setCode(newText)
  })
}

watchEffect(() => projectStore.module && applyEdits(projectStore.module, props.modelValue))

const data = computed(() => projectStore.module?.doc.data)
const text = computed(() => projectStore.module?.doc.getCode())

useObserveYjs(data, () => {
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
