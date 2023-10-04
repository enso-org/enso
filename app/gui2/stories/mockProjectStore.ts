import { defineStore } from 'pinia'
import { Awareness } from 'y-protocols/awareness'
import * as Y from 'yjs'

import type { useProjectStore as originalUseProjectStore } from '@/stores/project'
import { DistributedModule } from 'shared/yjsModel'
import { markRaw } from 'vue'

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
    name: 'Mock Project',
    awareness: markRaw(new Awareness(doc)),
    undoManager: markRaw(new Y.UndoManager([], { doc })),
    module: markRaw(new DistributedModule(doc)),
    contentRoots: Promise.resolve([]),
    lsRpcConnection: null as any,
  }
  return result
})
