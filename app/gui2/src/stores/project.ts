import { useGuiConfig, type GuiConfig } from '@/providers/guiConfig'
import { attachProvider } from '@/util/crdt'
import type { Opt } from '@/util/opt'
import { Client, RequestManager, WebSocketTransport } from '@open-rpc/client-js'
import { computedAsync } from '@vueuse/core'
import * as random from 'lib0/random'
import { defineStore } from 'pinia'
import { LanguageServer } from 'shared/languageServer'
import type { ContentRoot, ContextId, MethodPointer } from 'shared/languageServerTypes'
import { DistributedProject, type Uuid } from 'shared/yjsModel'
import { computed, markRaw, ref, watchEffect } from 'vue'
import { Awareness } from 'y-protocols/awareness'
import * as Y from 'yjs'

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
  }

  throw new Error('Incomplete engine configuration')
}

async function initializeLsRpcConnection(urls: LsUrls): Promise<{
  connection: LanguageServer
  contentRoots: ContentRoot[]
}> {
  const transport = new WebSocketTransport(urls.rpcUrl)
  const requestManager = new RequestManager([transport])
  const client = new Client(requestManager)
  const clientId = random.uuidv4() as Uuid
  const connection = new LanguageServer(client)
  const contentRoots = (await connection.initProtocolConnection(clientId)).contentRoots
  return { connection, contentRoots }
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
  const projectId = config.value.startup?.project
  if (projectId == null) throw new Error('Missing project ID')

  const lsUrls = resolveLsUrl(config.value)
  const initializedConnection = initializeLsRpcConnection(lsUrls)
  const lsRpcConnection = initializedConnection.then(({ connection }) => connection)
  const contentRoots = initializedConnection.then(({ contentRoots }) => contentRoots)

  const undoManager = new Y.UndoManager([], { doc })

  const name = computed(() => config.value.startup?.project)
  const namespace = computed(() => config.value.engine?.namespace)

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

  async function createExecutionContextForMain(): Promise<Opt<ExecutionContext>> {
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

  return {
    setObservedFileName(name: string) {
      observedFileName.value = name
    },
    createExecutionContextForMain,
    module,
    contentRoots,
    undoManager,
    awareness,
    lsRpcConnection: markRaw(lsRpcConnection),
  }
})
