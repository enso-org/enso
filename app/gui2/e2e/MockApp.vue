<script setup lang="ts">
import diff from 'fast-diff'
import { computed, watchEffect } from 'vue'
import GraphEditor from '../src/components/GraphEditor.vue'
import { useProjectStore } from '../src/stores/project'
import { getMainFile, setMainFile } from './mockEngine'

const projectStore = useProjectStore()
const mod = projectStore.projectModel.createNewModule('Main.enso')
mod.doc.ydoc.emit('load', [])

const mainFile = computed({
  get() {
    return getMainFile()
  },
  set(value) {
    setMainFile(value)
  },
})

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

watchEffect(() => projectStore.module && applyEdits(projectStore.module, mainFile.value))
</script>

<template>
  <GraphEditor />
</template>

<style scoped>
:is(.viewport) {
  color: var(--color-text);
  font-family: 'M PLUS 1', sans-serif;
  font-size: 11.5px;
  font-weight: 500;
  line-height: 20px;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  height: 100vh;
}
</style>
