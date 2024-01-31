<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { watch } from 'vue'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
projectStore.setObservedFileName('Main.enso')
mod.doc.ydoc.emit('load', [])
let syncedCode: string | undefined
watch(
  () => projectStore.module,
  (mod) => {
    if (!mod) return
    const syncModule = new Ast.MutableModule(mod.doc.ydoc)
    applyEdits(syncModule, props.modelValue)
    const _astModule = new Ast.ReactiveModule(syncModule, [
      (module, dirtyNodes) => {
        if (dirtyNodes.size === 0) return
        const root = module.root()
        if (root) {
          const { code } = Ast.print(root)
          if (code !== props.modelValue) {
            syncedCode = code
            emit('update:modelValue', code)
          }
        }
      },
    ])
    watch(
      () => props.modelValue,
      (modelValue) => applyEdits(syncModule, modelValue),
    )
  },
)

function applyEdits(syncModule: Ast.MutableModule, newText: string) {
  if (newText !== syncedCode) {
    syncModule.ydoc.transact(() => {
      syncModule.syncRoot(Ast.parseBlock(newText, syncModule))
    }, 'local')
  }
}
</script>

<template>
  <slot></slot>
</template>
