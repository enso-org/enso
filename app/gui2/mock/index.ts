import { provideGuiConfig } from '@/providers/guiConfig'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { GraphDb, mockNode } from '@/stores/graph/graphDatabase'
import { useProjectStore } from '@/stores/project'
import { ComputedValueRegistry } from '@/stores/project/computedValueRegistry'
import { MockTransport, MockWebSocket } from '@/util/net'
import * as random from 'lib0/random'
import { getActivePinia } from 'pinia'
import type { ExprId } from 'shared/yjsModel'
import { ref, type App } from 'vue'
import { mockDataHandler, mockLSHandler } from './engine'
export * as providers from './providers'
export * as vue from './vue'

export function languageServer() {
  MockTransport.addMock('engine', mockLSHandler)
}

export function dataServer() {
  MockWebSocket.addMock('data', mockDataHandler)
}

export function guiConfig(app: App) {
  return provideGuiConfig._mock(
    ref({
      startup: {
        project: 'Mock Project',
        displayedProjectName: 'Mock Project',
      },
      engine: { rpcUrl: 'mock://engine', dataUrl: 'mock://data' },
    }),
    app,
  )
}

export const computedValueRegistry = ComputedValueRegistry.Mock
export const graphDb = GraphDb.Mock

export function widgetRegistry(app: App) {
  return widgetRegistry.withGraphDb(graphDb())(app)
}

widgetRegistry.withGraphDb = function widgetRegistryWithGraphDb(graphDb: GraphDb) {
  return (app: App) => provideWidgetRegistry._mock([graphDb], app)
}

export function graphStore() {
  return useGraphStore(getActivePinia())
}

type ProjectStore = ReturnType<typeof projectStore>

export function projectStore() {
  const projectStore = useProjectStore(getActivePinia())
  const mod = projectStore.projectModel.createNewModule('Main.enso')
  mod.doc.ydoc.emit('load', [])
  mod.doc.contents.insert(0, 'main =\n')
  return projectStore
}

/** The stores should be initialized in this order, as `graphStore` depends on `projectStore`. */
export function projectStoreAndGraphStore() {
  return [projectStore(), graphStore()] satisfies [] | unknown[]
}

export function newExprId() {
  return random.uuidv4() as ExprId
}

/** This should only be used for supplying as initial props when testing.
 * Please do {@link GraphDb.mockNode} with a `useGraphStore().db` after mount. */
export function node() {
  return mockNode()
}

export function waitForMainModule(projectStore?: ProjectStore) {
  const definedProjectStore = projectStore ?? useProjectStore(getActivePinia())
  return new Promise((resolve, reject) => {
    const handle1 = window.setInterval(() => {
      if (definedProjectStore.module != null) {
        window.clearInterval(handle1)
        window.clearTimeout(handle2)
        resolve(definedProjectStore.module)
      }
    }, 10)
    const handle2 = window.setTimeout(() => {
      window.clearInterval(handle1)
      reject()
    }, 5_000)
  })
}
