import { ref, watchEffect } from 'vue'
import { defineStore } from 'pinia'
import * as Y from 'yjs'
import { attachProvider } from '@/util/crdt'
import { DistributedProject } from 'shared/yjs-model'
import { computedAsync } from '@vueuse/core'
import { Awareness } from 'y-protocols/awareness'
import { modKey, useWindowEvent } from '@/util/events'
import { useGuiConfig, type GuiConfig } from '@/providers/guiConfig'

interface LsUrls {
  rpcUrl: string
  dataUrl: string
}

function resolveLsUrl(config: GuiConfig): LsUrls {
  const engine = config.engine
  if (engine == null) throw new Error('Missing engine configuration')

  if (engine.rpcUrl != null && engine.dataUrl != null) {
    return {
      rpcUrl: engine.rpcUrl,
      dataUrl: engine.dataUrl,
    }
  } else if (engine.projectManagerUrl != null) {
    throw new Error('Project manager connection not implemented')
  }

  throw new Error('Incomplete engine configuration')
}

/**
 * The project store synchronizes and holds the open project-related data. The synchronization is
 * performed using a CRDT data types from Yjs. Once the data is synchronized with a "LS bridge"
 * client, it is submitted to the language server as a document update.
 */
export const useProjectStore = defineStore('project', () => {
  // inputs
  const observedFileName = ref<string>()

  const doc = new Y.Doc()
  const awareness = new Awareness(doc)

  const config = useGuiConfig()
  const projectId = config.value.startup?.project
  if (projectId == null) throw new Error('Missing project ID')

  const lsUrls = resolveLsUrl(config.value)

  const undoManager = new Y.UndoManager([], { doc })

  useWindowEvent('keydown', (e) => {
    if (modKey(e) && e.key === 'z') {
      console.log('undo')
      undoManager.undo()
    } else if (modKey(e) && e.key === 'y') {
      console.log('redo')
      undoManager.redo()
    }
  })

  watchEffect((onCleanup) => {
    // For now, let's assume that the websocket server is running on the same host as the web server.
    // Eventually, we can make this configurable, or even runtime variable.
    // const socketUrl = location.origin.replace(/^http/, 'ws') + '/project'
    const socketUrl = new URL(location.origin)
    socketUrl.protocol = location.protocol.replace(/^http/, 'ws')
    socketUrl.pathname = '/project'
    const provider = attachProvider(socketUrl.href, 'root', { ls: lsUrls.rpcUrl }, doc, awareness)
    onCleanup(() => {
      provider.dispose()
    })
  })

  // const model = new DistributedModel(doc)
  const projectModel = new DistributedProject(doc)

  const module = computedAsync(async () => {
    const moduleName = observedFileName.value
    if (moduleName == null) return
    console.log('module name', moduleName)
    return await projectModel.openOrCreateModule(moduleName)
  })

  watchEffect((onCleanup) => {
    const mod = module.value
    if (mod == null) return
    const undoable: typeof undoManager.scope = [mod.contents, mod.name, mod.idMap]
    undoManager.scope.push(...undoable)
    onCleanup(() => {
      undoManager.scope = undoManager.scope.filter((s) => !undoable.includes(s))
    })
  })

  return {
    setObservedFileName(name: string) {
      observedFileName.value = name
    },
    module,
    undoManager,
  }
})
