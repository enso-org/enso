import { useGuiConfig, type GuiConfig } from '@/providers/guiConfig'
import { attachProvider } from '@/util/crdt'
import { Client, RequestManager, WebSocketTransport } from '@open-rpc/client-js'
import { computedAsync } from '@vueuse/core'
import * as random from 'lib0/random'
import { defineStore } from 'pinia'
import { DataServer } from 'shared/dataServer'
import { LanguageServer } from 'shared/languageServer'
import { WebsocketClient } from 'shared/websocket'
import { DistributedProject, type ExprId, type Uuid } from 'shared/yjsModel'
import {
  markRaw,
  onMounted,
  onUnmounted,
  ref,
  shallowRef,
  watch,
  watchEffect,
  type Ref,
  type ShallowRef,
} from 'vue'
import { Awareness } from 'y-protocols/awareness'
import * as Y from 'yjs'
import { OutboundPayload, VisualizationUpdate } from '../../shared/binaryProtocol'
import type { ContextId, VisualizationConfiguration } from '../../shared/languageServerTypes'
import { DEFAULT_VISUALIZATION_CONFIGURATION } from './visualization'

interface LsUrls {
  rpcUrl: string
  dataUrl: string
}

export type NodeVisualizationConfiguration = Omit<
  VisualizationConfiguration,
  'executionContextId' | 'visualizationModule'
>

const MAIN_DEFINITION_NAME = 'main'
/** Endpoint used by default by a locally run Project Manager. */
const PROJECT_MANAGER_ENDPOINT = 'ws://127.0.0.1:30535'
/** Default project name used by IDE on startup. */
const DEFAULT_PROJECT_NAME = 'Unnamed'
/** The default namespace used when opening a project. */
const DEFAULT_PROJECT_NAMESPACE = 'local'
/** Visualization folder where IDE can look for user-defined visualizations per project. */
const VISUALIZATION_DIRECTORY = 'visualization'
/** How many times IDE will try attaching visualization when there is a timeout error.
 *
 * Timeout error suggests that there might be nothing wrong with the request, just that the backend
 * is currently too busy to reply or that there is some connectivity hiccup. Thus, it makes sense
 * to give it a few more tries. */
const ATTACHING_TIMEOUT_RETRIES = 50

function resolveProjectName(config: GuiConfig): string {
  const projectName = config.startup?.project
  if (projectName == null) {
    throw new Error('Missing project name')
  }
  return projectName
}

function resolveLsUrl(config: GuiConfig): LsUrls {
  const engine = config.engine
  if (engine == null) throw new Error('Missing engine configuration')

  if (engine.rpcUrl != null && engine.dataUrl != null) {
    return {
      rpcUrl: engine.rpcUrl,
      dataUrl: engine.dataUrl,
    }
  }

  throw new Error('Incomplete engine configuration')
}

/**
 * The project store synchronizes and holds the open project-related data. The synchronization is
 * performed using a CRDT data types from Yjs. Once the data is synchronized with a "LS bridge"
 * client, it is submitted to the language server as a document update.
 */
export const useProjectStore = defineStore('project', () => {
  const observedFileName = ref<string>()

  const doc = new Y.Doc()
  const awareness = new Awareness(doc)

  const config = useGuiConfig()
  const projectId = config.value.startup?.project
  if (projectId == null) throw new Error('Missing project ID')

  const name = resolveProjectName(config.value)
  const lsUrls = resolveLsUrl(config.value)

  const rpcTransport = new WebSocketTransport(lsUrls.rpcUrl)
  const rpcRequestManager = new RequestManager([rpcTransport])
  const rpcClient = new Client(rpcRequestManager)
  const lsRpcConnection = new LanguageServer(rpcClient)
  const dataClient = new WebsocketClient(lsUrls.dataUrl, {
    binaryType: 'arraybuffer',
    sendPings: false,
  })
  const dataConnection = new DataServer(dataClient)

  const undoManager = new Y.UndoManager([], { doc })

  watchEffect((onCleanup) => {
    // For now, let's assume that the websocket server is running on the same host as the web server.
    // Eventually, we can make this configurable, or even runtime variable.
    const socketUrl = new URL(location.origin)
    socketUrl.protocol = location.protocol.replace(/^http/, 'ws')
    socketUrl.pathname = '/project'
    const provider = attachProvider(socketUrl.href, 'index', { ls: lsUrls.rpcUrl }, doc, awareness)
    onCleanup(() => {
      provider.dispose()
    })
  })

  const projectModel = new DistributedProject(doc)
  const moduleDocGuid = ref<string>()

  function currentDocGuid() {
    const name = observedFileName.value
    if (name == null) return
    return projectModel.modules.get(name)?.guid
  }
  function tryReadDocGuid() {
    const guid = currentDocGuid()
    if (guid === moduleDocGuid.value) return
    moduleDocGuid.value = guid
  }

  projectModel.modules.observe((_) => tryReadDocGuid())
  watchEffect(tryReadDocGuid)

  const module = computedAsync(async () => {
    const guid = moduleDocGuid.value
    if (guid == null) return null
    const moduleName = projectModel.findModuleByDocId(guid)
    if (moduleName == null) return null
    return await projectModel.openModule(moduleName)
  })

  watchEffect((onCleanup) => {
    const mod = module.value
    if (mod == null) return
    const scope: typeof undoManager.scope = [mod.contents, mod.idMap]
    undoManager.scope.push(...scope)
    onCleanup(() => {
      undoManager.scope = undoManager.scope.filter((s) => !scope.includes(s))
    })
  })

  const clientId = random.uuidv4() as Uuid
  const executionContextId = random.uuidv4() as ContextId
  const mainModuleId = ref<Uuid>()
  const mainModule = `${DEFAULT_PROJECT_NAMESPACE}.${name}.Main`

  async function initializeServers() {
    await dataConnection.initialize(clientId)
    const projectInfo = await lsRpcConnection.initProtocolConnection(clientId)
    const projectRoot = projectInfo.contentRoots.find((root) => root.type === 'Project')
    if (projectRoot == null) {
      console.error('Protocol connection initialization did not return a project root.')
      return
    }
    mainModuleId.value = projectRoot.id
    await lsRpcConnection.createExecutionContext(executionContextId)
    await lsRpcConnection.pushExecutionContextItem(executionContextId, {
      type: 'ExplicitCall',
      methodPointer: {
        module: mainModule,
        definedOnType: mainModule,
        name: MAIN_DEFINITION_NAME,
      },
      thisArgumentExpression: null,
      positionalArgumentsExpressions: [],
    })
  }

  const initializeServersPromise = initializeServers()

  function useVisualizationData(
    expressionId: ExprId,
    configuration: Ref<NodeVisualizationConfiguration | undefined>,
    visible: Ref<boolean>,
  ): ShallowRef<{} | undefined> {
    const id = random.uuidv4() as Uuid
    const visualizationData = shallowRef<{}>()

    watch(visible, async (visible) => {
      if (visible) {
        await initializeServersPromise
        await lsRpcConnection.attachVisualization(id, expressionId, {
          executionContextId,
          visualizationModule: mainModule,
          ...(configuration.value ?? DEFAULT_VISUALIZATION_CONFIGURATION),
        })
      } else {
        await initializeServersPromise
        await lsRpcConnection.detachVisualization(id, expressionId, executionContextId)
      }
    })

    watchEffect(async () => {
      if (configuration.value == null || !visible.value) {
        return
      }
      await initializeServersPromise
      await lsRpcConnection.modifyVisualization(id, {
        ...configuration.value,
        executionContextId,
        visualizationModule: mainModule,
      })
    })

    function onVisualizationUpdate(vizUpdate: VisualizationUpdate) {
      const json = vizUpdate.dataString()
      const newData = json != null ? JSON.parse(json) : undefined
      visualizationData.value = newData
    }

    onMounted(() => {
      dataConnection.on(
        `${OutboundPayload.VISUALIZATION_UPDATE}:${id as string}`,
        onVisualizationUpdate,
      )
    })

    onUnmounted(() => {
      dataConnection.off(`${OutboundPayload.VISUALIZATION_UPDATE}:${id}`, onVisualizationUpdate)
    })

    return visualizationData
  }

  return {
    setObservedFileName(name: string) {
      observedFileName.value = name
    },
    module,
    namespace: DEFAULT_PROJECT_NAMESPACE,
    name,
    undoManager,
    lsRpcConnection: markRaw(lsRpcConnection),
    dataConnection: markRaw(dataConnection),
    useVisualizationData,
  }
})
