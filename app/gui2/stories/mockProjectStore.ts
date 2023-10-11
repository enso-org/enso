import type { ExecutionContext, useProjectStore as originalUseProjectStore } from '@/stores/project'
import { ComputedValueRegistry } from '@/util/computedValueRegistry'
import { ObservableV2 } from 'lib0/observable'
import { defineStore } from 'pinia'
import { DistributedModule } from 'shared/yjsModel'
import { markRaw, ref } from 'vue'
import { Awareness } from 'y-protocols/awareness'
import * as Y from 'yjs'

export const useProjectStore = defineStore('project', () => {
  const doc = new Y.Doc()
  const mockExecutionContext = new ObservableV2() as ExecutionContext
  const result: Omit<
    ReturnType<typeof originalUseProjectStore>,
    `$${string}` | `_customProperties`
  > = {
    setObservedFileName() {},
    name: 'Mock Project',
    awareness: markRaw(new Awareness(doc)),
    module: markRaw(new DistributedModule(doc)),
    contentRoots: Promise.resolve([]),
    lsRpcConnection: null as any,
    dataConnection: null as any,
    executionContext: mockExecutionContext,
    computedValueRegistry: new ComputedValueRegistry(mockExecutionContext),
    stopCapturingUndo() {},
    useVisualizationData() {
      return ref({})
    },
  }
  return result
})
