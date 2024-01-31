<script setup lang="ts">
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { ref, shallowRef, watch, watchEffect } from 'vue'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
projectStore.setObservedFileName('Main.enso')
mod.doc.ydoc.emit('load', [])
const syncModule = shallowRef<Ast.MutableModule>()
let syncedCode: string | undefined
watch(
  () => projectStore.module,
  (mod) => {
    if (!mod) return
    console.info(`new projectStore.module`)
    syncModule.value = new Ast.MutableModule(mod.doc.ydoc)
    const _astModule = new Ast.ReactiveModule(syncModule.value, [
      (module, dirtyNodes) => {
        if (dirtyNodes.size === 0) return
        const root = module.root()
        if (root) {
          const { code } = Ast.print(root)
          if (code !== props.modelValue) {
            console.info(`update:modelValue`, code)
            emit('update:modelValue', code)
            syncedCode = code
          }
        }
      },
    ])
  },
)

function applyEdits(syncModule: Ast.MutableModule, newText: string) {
  if (newText !== syncedCode) {
    console.info(`applyEdits`, syncedCode, newText)
    syncModule.ydoc.transact(() => {
      syncModule.syncRoot(Ast.parseBlock(newText, syncModule))
    }, 'mock:applyEdits')
  }
}

watchEffect(() => syncModule.value && applyEdits(syncModule.value, props.modelValue))
</script>

<template>
  <slot></slot>
</template>
