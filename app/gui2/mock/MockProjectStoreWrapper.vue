<script setup lang="ts">
/**
 * Mock project store, used in some histoire stories.
 */

import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { reactive, watch } from 'vue'
import { SourceDocument } from 'ydoc-shared/ast/sourceDocument'

const props = defineProps<{ modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
projectStore.setObservedFileName('Main.enso')
mod.doc.ydoc.emit('load', [mod.doc.ydoc])
let syncedCode: string | undefined
watch(
  () => projectStore.module,
  (mod) => {
    if (!mod) return
    const syncModule = new Ast.MutableModule(mod.doc.ydoc)
    applyEdits(syncModule, props.modelValue)
    const moduleSource = reactive(SourceDocument.Empty())
    syncModule.observe((update) => moduleSource.applyUpdate(syncModule, update))
    watch(
      () => moduleSource.text,
      (text) => {
        if (text !== props.modelValue) {
          syncedCode = text
          emit('update:modelValue', text)
        }
      },
    )
    watch(
      () => props.modelValue,
      (modelValue) => applyEdits(syncModule, modelValue),
    )
  },
)

function applyEdits(syncModule: Ast.MutableModule, newText: string) {
  if (newText !== syncedCode) {
    syncModule.transact(() => {
      syncModule.syncRoot(Ast.parseBlock(newText, syncModule))
    })
  }
}
</script>

<template>
  <slot></slot>
</template>
