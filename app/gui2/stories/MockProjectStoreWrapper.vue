<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { ref, watch, watchEffect } from 'vue'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
projectStore.setObservedFileName('Main.enso')
mod.doc.ydoc.emit('load', [])
const syncModule = ref<Ast.MutableModule>()
watch(
  () => projectStore.module,
  (mod) => {
    if (!mod) return
    syncModule.value = new Ast.MutableModule(mod.doc.ydoc)
    const _astModule = new Ast.ReactiveModule(syncModule.value, [
      (module) => {
        const root = module.root()
        if (root) {
          const { code } = Ast.print(root)
          if (code !== props.modelValue) emit('update:modelValue', code)
        }
      },
    ])
  },
)

function applyEdits(syncModule: Ast.MutableModule, newText: string) {
  syncModule.ydoc.transact(() => {
    syncModule.syncRoot(Ast.parseBlock(newText, syncModule))
  })
}

watchEffect(() => syncModule.value && applyEdits(syncModule.value, props.modelValue))
</script>

<template>
  <slot></slot>
</template>
