<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { watchEffect } from 'vue'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
projectStore.setObservedFileName('Main.enso')
mod.doc.ydoc.emit('load', [])
const syncModule = new Ast.MutableModule(mod.doc.ydoc)

function applyEdits(module: NonNullable<typeof projectStore.module>, newText: string) {
  module.transact(() => {
    syncModule.syncRoot(Ast.parseBlock(newText, syncModule))
  })
}

watchEffect(() => projectStore.module && applyEdits(projectStore.module, props.modelValue))

const astModule = new Ast.ReactiveModule(syncModule)
astModule.onUpdate(() => {
  const root = astModule.root()
  if (root) {
    const { code } = Ast.print(root)
    if (code !== props.modelValue) emit('update:modelValue', code)
  }
})
</script>

<template>
  <slot></slot>
</template>
