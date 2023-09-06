import { ref, watchEffect } from 'vue'
import { defineStore } from 'pinia'
import * as Y from 'yjs'
import { attachProvider } from '@/util/crdt'
import { DistributedModel } from 'shared/yjs-model'
import { computedAsync } from '@vueuse/core'
import { Awareness } from 'y-protocols/awareness'

/**
 * The project store synchronizes and holds the open project-related data. The synchronization is
 * performed using a CRDT data types from Yjs. Once the data is synchronized with a "LS bridge"
 * client, it is submitted to the language server as a document update.
 */
export const useProjectStore = defineStore('project', () => {
  // inputs
  const projectName = ref<string>()
  const observedFileName = ref<string>()

  const doc = new Y.Doc()
  const awareness = new Awareness(doc)

  watchEffect((onCleanup) => {
    // For now, let's assume that the websocket server is running on the same host as the web server.
    // Eventually, we can make this configurable, or even runtime variable.
    const socketUrl = location.origin.replace(/^http/, 'ws') + '/room'
    const provider = attachProvider(socketUrl, 'enso-projects', doc, awareness)
    onCleanup(() => {
      provider.dispose()
    })
  })

  const model = new DistributedModel(doc)
  const project = computedAsync(async () => {
    const name = projectName.value
    if (name == null) return
    return await model.openOrCreateProject(name)
  })

  const module = computedAsync(async () => {
    const moduleName = observedFileName.value
    const p = project.value
    if (moduleName == null || p == null) return
    return await p.openOrCreateModule(moduleName)
  })

  return {
    setProjectName(name: string) {
      projectName.value = name
    },
    setObservedFileName(name: string) {
      observedFileName.value = name
    },
    module,
  }
})
