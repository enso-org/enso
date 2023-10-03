import { defineStore } from 'pinia'
import { Awareness } from 'y-protocols/awareness'
import * as Y from 'yjs'
import { DistributedModule } from '../shared/yjsModel'
import type { useProjectStore as originalUseProjectStore } from '../src/stores/project'

export const useProjectStore = defineStore('project', () => {
  const doc = new Y.Doc()
  const result: Omit<
    ReturnType<typeof originalUseProjectStore>,
    `$${string}` | `_customProperties`
  > = {
    setObservedFileName() {},
    async createExecutionContextForMain() {
      return undefined
    },
    awareness: new Awareness(doc),
    undoManager: new Y.UndoManager([], { doc }),
    module: new DistributedModule(doc),
    contentRoots: Promise.resolve([]),
    lsRpcConnection: null as any,
  }
  return result
})
