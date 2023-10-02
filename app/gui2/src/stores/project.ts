import { useGuiConfig,type GuiConfig } from '@/providers/guiConfig'
import { DEFAULT_VISUALIZATION_CONFIGURATION } from '@/stores/visualization'
import { attachProvider } from '@/util/crdt'
import { Client,RequestManager,WebSocketTransport } from '@open-rpc/client-js'
import { computedAsync } from '@vueuse/core'
import * as random from 'lib0/random'
import { defineStore } from 'pinia'
import { OutboundPayload,VisualizationUpdate } from 'shared/binaryProtocol'
import { DataServer } from 'shared/dataServer'
import { LanguageServer } from 'shared/languageServer'
import type {
ContentRoot,
ContextId,
MethodPointer,
VisualizationConfiguration,
} from 'shared/languageServerTypes'
import { WebsocketClient } from 'shared/websocket'
import { DistributedProject,type ExprId,type Uuid } from 'shared/yjsModel'
import {
computed,
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

async function initializeLsRpcConnection(
  clientId: Uuid,
  url: string,
): Promise<{
  connection: LanguageServer
  contentRoots: ContentRoot[]
}> {
  const transport = new WebSocketTransport(url)
  const requestManager = new RequestManager([transport])
  const client = new Client(requestManager)
  const connection = new LanguageServer(client)
  const contentRoots = (await connection.initProtocolConnection(clientId)).contentRoots
  return { connection, contentRoots }
}

async function initializeDataConnection(clientId: Uuid, url: string) {
  const client = new WebsocketClient(url, { binaryType: 'arraybuffer', sendPings: false })
  const connection = new DataServer(client)
  await connection.initialize(clientId)
  return connection
}

/**
 * Execution Context
 *
 * This class represent an execution context created in the Language Server. It creates
 * it and pushes the initial frame upon construction.
 *
 * It hides the asynchronous nature of the language server. Each call is scheduled and
 * run only when the previous call is done.
 */
export class ExecutionContext {
  state: Promise<{ lsRpc: LanguageServer; id: ContextId }>

  constructor(
    lsRpc: Promise<LanguageServer>,
    call: {
      methodPointer: MethodPointer
      thisArgumentExpression?: string
      positionalArgumentsExpressions?: string[]
    },
  ) {
    this.state = lsRpc.then(async (lsRpc) => {
      const { contextId } = await lsRpc.createExecutionContext()
      await lsRpc.pushExecutionContextItem(contextId, {
        type: 'ExplicitCall',
        positionalArgumentsExpressions: call.positionalArgumentsExpressions ?? [],
        ...call,
      })
      return { lsRpc, id: contextId }
    })
  }

  destroy() {
    this.state = this.state.then(({ lsRpc, id }) => {
      lsRpc.destroyExecutionContext(id)
      return { lsRpc, id }
    })
  }
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
  const projectName = config.value.startup?.project
  if (projectName == null) throw new Error('Missing project name.')

  const clientId = random.uuidv4() as Uuid
  const lsUrls = resolveLsUrl(config.value)
  const initializedConnection = initializeLsRpcConnection(clientId, lsUrls.rpcUrl)
  const lsRpcConnection = initializedConnection.then(({ connection }) => connection)
  const contentRoots = initializedConnection.then(({ contentRoots }) => contentRoots)
  const dataConnection = initializeDataConnection(clientId, lsUrls.dataUrl)

  const undoManager = new Y.UndoManager([], { doc })

  const name = computed(() => config.value.startup?.project)
  const namespace = computed(() => config.value.engine?.namespace)

  const mainModule = computed(() => `${DEFAULT_PROJECT_NAMESPACE}.${name.value}.Main`)

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
    const scope: typeof undoManager.scope = [mod.doc.contents, mod.doc.idMap]
    undoManager.scope.push(...scope)
    onCleanup(() => {
      undoManager.scope = undoManager.scope.filter((s) => !scope.includes(s))
    })
  })

  async function createExecutionContextForMain(): Promise<ExecutionContext | undefined> {
    if (name.value == null) {
      console.error('Cannot create execution context. Unknown project name.')
      return
    }
    if (namespace.value == null) {
      console.warn(
        'Unknown project\'s namespace. Assuming "local", however it likely won\'t work in cloud',
      )
    }
    const projectName = `${namespace.value ?? 'local'}.${name.value}`
    const mainModule = `${projectName}.Main`
    const projectRoot = (await contentRoots).find((root) => root.type === 'Project')
    if (projectRoot == null) {
      console.error(
        'Cannot create execution context. Protocol connection initialization did not return a project root.',
      )
      return
    }
    return new ExecutionContext(lsRpcConnection, {
      methodPointer: { module: mainModule, definedOnType: mainModule, name: 'main' },
    })
  }

  const executionContextRef = shallowRef<ExecutionContext>()
  const executionContext = lsRpcConnection
    .then(createExecutionContextForMain)
    .then((executionContext) => {
      executionContextRef.value = executionContext
      return executionContext
    })
  const executionContextId = executionContext
    .then((context) => context?.state)
    .then((state) => state?.id)

  function useVisualizationData(
    expressionId: ExprId,
    configuration: Ref<NodeVisualizationConfiguration | undefined>,
    visible: Ref<boolean>,
  ): ShallowRef<{} | undefined> {
    const id = random.uuidv4() as Uuid
    const visualizationData = shallowRef<{}>()

    watch(visible, async (visible) => {
      if (visible) {
        await (
          await lsRpcConnection
        ).attachVisualization(id, expressionId, {
          executionContextId: (await executionContextId)!,
          visualizationModule: mainModule.value,
          ...(configuration.value ?? DEFAULT_VISUALIZATION_CONFIGURATION),
        })
      } else {
        await (
          await lsRpcConnection
        ).detachVisualization(id, expressionId, (await executionContextId)!)
      }
    })

    watchEffect(async () => {
      if (!visible.value) return
      const mainModule_ = mainModule.value
      const config = configuration.value
      await (
        await lsRpcConnection
      ).modifyVisualization(id, {
        executionContextId: (await executionContextId)!,
        visualizationModule: mainModule_,
        ...(config ?? DEFAULT_VISUALIZATION_CONFIGURATION),
      })
    })

    function onVisualizationUpdate(vizUpdate: VisualizationUpdate) {
      const json = vizUpdate.dataString()
      const newData = json != null ? JSON.parse(json) : undefined
      visualizationData.value = newData
    }

    onMounted(async () => {
      ;(await dataConnection).on(
        `${OutboundPayload.VISUALIZATION_UPDATE}:${id as string}`,
        onVisualizationUpdate,
      )
    })

    onUnmounted(async () => {
      ;(await dataConnection).off(
        `${OutboundPayload.VISUALIZATION_UPDATE}:${id}`,
        onVisualizationUpdate,
      )
    })

    return visualizationData
  }

  return {
    setObservedFileName(name: string) {
      observedFileName.value = name
    },
    name: projectName,
    createExecutionContextForMain,
    executionContext: executionContextRef,
    module,
    contentRoots,
    undoManager,
    awareness,
    lsRpcConnection: markRaw(lsRpcConnection),
    dataConnection: markRaw(dataConnection),
    useVisualizationData,
  }
})
