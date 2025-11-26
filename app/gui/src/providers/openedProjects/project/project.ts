import { useOpenedProjects } from '$/providers/openedProjects'
import { ComputedValueRegistry } from '$/providers/openedProjects/project/computedValueRegistry'
import {
  ExecutionContext,
  visualizationConfigPreprocessorEqual,
  type NodeVisualizationConfiguration,
} from '$/providers/openedProjects/project/executionContext'
import { VisualizationDataRegistry } from '$/providers/openedProjects/project/visualizationDataRegistry'
import { type ProjectNameStore } from '$/providers/openedProjects/projectNames'
import { Awareness } from '@/stores/awareness'
import { attachProvider, useObserveYjs } from '@/util/crdt'
import { nextEvent } from '@/util/data/observable'
import type { Opt } from '@/util/data/opt'
import { ReactiveMapping } from '@/util/database/reactiveDb'
import type { MethodPointer } from '@/util/methodPointer'
import { createDataWebsocket, createRpcTransport, useAbortScope } from '@/util/net'
import { DataServer } from '@/util/net/dataServer'
import { ProjectPath } from '@/util/projectPath'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { proxyRefs } from '@/util/reactivity'
import { computedAsync } from '@vueuse/core'
import { ProjectId } from 'enso-common/src/services/Backend'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { wait } from 'lib0/promise'
import type { Ref, WatchSource } from 'vue'
import {
  computed,
  markRaw,
  onScopeDispose,
  ref,
  shallowRef,
  watch,
  watchEffect,
  type WritableComputedRef,
} from 'vue'
import type { Identifier } from 'ydoc-shared/ast'
import { OutboundPayload, VisualizationUpdate } from 'ydoc-shared/binaryProtocol'
import { LanguageServer } from 'ydoc-shared/languageServer'
import type { Diagnostic, ExpressionId } from 'ydoc-shared/languageServerTypes'
import type { AbortScope } from 'ydoc-shared/util/net'
import {
  DistributedProject,
  localUserActionOrigins,
  type ExternalId,
  type Uuid,
} from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'

export interface LsUrls {
  rpcUrl: string
  dataUrl: string
  ydocUrl: string
}

const VISUALIZATION_PREPROCESSOR_PATH = ProjectPath.create(
  'Standard.Visualization' as QualifiedName,
  'Preprocessor' as Identifier,
)

export type ProjectStore = ReturnType<typeof createProjectStore>

/**
 * The project store synchronizes and holds the open project-related data. The synchronization is
 * performed using a CRDT data types from Yjs. Once the data is synchronized with a "LS bridge"
 * client, it is submitted to the language server as a document update.
 */
export function createProjectStore(
  props: {
    projectId: ProjectId
    projectAssetId: ProjectId
    engine: LsUrls
  },
  projectNames: ProjectNameStore,
) {
  const { projectId, projectAssetId } = props
  const abort = useAbortScope()
  const openedProjects = useOpenedProjects()

  const observedFileName = ref<string>()

  const doc = new Y.Doc()
  const awareness = new Awareness(doc)

  const clientId = crypto.randomUUID() as Uuid
  const lsRpcConnection = createLsRpcConnection(clientId, props.engine.rpcUrl, abort)
  const projectRootId = lsRpcConnection.contentRoots.then(
    (roots) => roots.find((root) => root.type === 'Project')?.id,
  )

  const dataConnection = initializeDataConnection(clientId, props.engine.dataUrl, abort)
  const rpcUrl = new URL(props.engine.rpcUrl)
  const isOnLocalBackend =
    rpcUrl.protocol === 'mock:' ||
    rpcUrl.hostname === 'localhost' ||
    rpcUrl.hostname === '127.0.0.1' ||
    rpcUrl.hostname === '[::1]' ||
    rpcUrl.hostname === '0:0:0:0:0:0:0:1'

  const moduleProjectPath = computed((): Result<ProjectPath> | undefined => {
    const filePath = observedFileName.value
    if (filePath == null) return undefined
    const withoutFileExt = filePath.replace(/\.enso$/, '')
    const withDotSeparators = withoutFileExt.replace(/\//g, '.')
    const qn = tryQualifiedName(withDotSeparators)
    if (!qn.ok) return qn
    return Ok(ProjectPath.create(undefined, qn.value))
  })

  const ydocUrl = resolveYDocUrl(props.engine.rpcUrl, props.engine.ydocUrl)
  let yDocsProvider: ReturnType<typeof attachProvider> | undefined
  watchEffect((onCleanup) => {
    yDocsProvider = attachProvider(
      ydocUrl.href,
      'index',
      { ls: props.engine.rpcUrl },
      doc,
      awareness.internal,
    )
    onCleanup(() => {
      yDocsProvider?.dispose()
      yDocsProvider = undefined
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

  projectModel.modules.observe(tryReadDocGuid)
  watchEffect(tryReadDocGuid)

  const module = computedAsync(
    async () => {
      const guid = moduleDocGuid.value
      if (guid == null) return null
      const moduleName = projectModel.findModuleByDocId(guid)
      if (moduleName == null) return null
      const mod = await projectModel.openModule(moduleName)
      for (const origin of localUserActionOrigins) mod?.undoManager.addTrackedOrigin(origin)
      return mod ? markRaw(mod) : null
    },
    undefined,
    { onError: console.error },
  )

  const entryPoint = computed<MethodPointer>(() => {
    const mainModule = ProjectPath.create(undefined, 'Main' as Identifier)
    return { module: mainModule, definedOnType: mainModule, name: 'main' as Identifier }
  })

  function createExecutionContextForMain(): ExecutionContext {
    return new ExecutionContext(
      lsRpcConnection,
      {
        methodPointer: entryPoint.value,
        positionalArgumentsExpressions: [],
      },
      abort,
      projectNames,
    )
  }

  const firstExecution = nextEvent(lsRpcConnection, 'executionContext/executionComplete').catch(
    (error) => {
      console.error('First execution failed:', error)
      throw error
    },
  )
  const executionContext = createExecutionContextForMain()
  const visualizationDataRegistry = new VisualizationDataRegistry(executionContext, dataConnection)
  const computedValueRegistry = ComputedValueRegistry.WithExecutionContext(
    executionContext,
    projectNames,
  )

  const diagnostics = shallowRef<Diagnostic[]>([])
  executionContext.on('executionStatus', (newDiagnostics) => {
    diagnostics.value = newDiagnostics
  })

  function useVisualizationData(
    configuration: WatchSource<Opt<NodeVisualizationConfiguration>>,
  ): Ref<Result<unknown> | null> {
    const visId = ref<Uuid>()

    watch(
      configuration,
      (config, oldConfig, onCleanup) => {
        if (!config) {
          visId.value = undefined
          return
        }
        // Regenerate the visualization ID when the preprocessor changes.
        if (!visualizationConfigPreprocessorEqual(config, oldConfig))
          visId.value = crypto.randomUUID()
        const id = visId.value!
        executionContext.setVisualization(id, config)
        onCleanup(() => executionContext.setVisualization(id, null))
      },
      // Make sure to flush this watch in 'post', otherwise it might cause operations on stale
      // ASTs just before the widget tree renders and cleans up the associated widget instances.
      { immediate: true, flush: 'post' },
    )

    return computed(() =>
      visId.value == null ?
        null
      : parseVisualizationData(visualizationDataRegistry.getRawData(visId.value)),
    )
  }

  const dataflowErrors = new ReactiveMapping(computedValueRegistry.db, (id, info) => {
    const config = computed(() =>
      info.payload.type === 'DataflowError' ?
        {
          expressionId: id,
          visualizationModule: 'Standard.Visualization.Preprocessor',
          expression: {
            module: VISUALIZATION_PREPROCESSOR_PATH,
            definedOnType: VISUALIZATION_PREPROCESSOR_PATH,
            name: 'error_preprocessor' as Identifier,
          },
        }
      : null,
    )
    const data = useVisualizationData(config)
    return computed<{ kind: 'Dataflow'; message: string } | undefined>(() => {
      const visResult = data.value
      if (!visResult) return
      if (!visResult.ok) {
        visResult.error.log('Dataflow Error visualization evaluation failed')
        return undefined
      } else if (
        visResult.value != null &&
        typeof visResult.value === 'object' &&
        'message' in visResult.value &&
        typeof visResult.value.message === 'string'
      ) {
        if ('kind' in visResult.value && visResult.value.kind === 'Dataflow')
          return { kind: visResult.value.kind, message: visResult.value.message }
        // Other kinds of error are not handled here
        else return undefined
      } else {
        console.error('Invalid dataflow error payload:', visResult.value)
        return undefined
      }
    })
  })

  const isRecordingEnabled = computed(() => executionMode.value === 'live')

  function stopCapturingUndo() {
    module.value?.undoManager.stopCapturing()
  }

  function executeExpression(
    expressionId: ExternalId,
    expression: string,
  ): Promise<Result<any> | null> {
    return new Promise((resolve) => {
      const visualizationId = crypto.randomUUID() as Uuid
      const dataHandler = (visData: VisualizationUpdate, uuid: Uuid | null) => {
        if (uuid === visualizationId) {
          dataConnection.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, dataHandler)
          executionContext.off('visualizationEvaluationFailed', errorHandler)
          const dataStr = Ok(visData.dataString())
          resolve(parseVisualizationData(dataStr))
        }
      }
      const errorHandler = (
        uuid: Uuid,
        _expressionId: ExpressionId,
        message: string,
        _diagnostic: Diagnostic | undefined,
      ) => {
        if (uuid == visualizationId) {
          resolve(Err(message))
          dataConnection.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, dataHandler)
          executionContext.off('visualizationEvaluationFailed', errorHandler)
        }
      }
      dataConnection.on(`${OutboundPayload.VISUALIZATION_UPDATE}`, dataHandler)
      executionContext.on('visualizationEvaluationFailed', errorHandler)
      return lsRpcConnection.executeExpression(
        executionContext.id,
        visualizationId,
        expressionId,
        expression,
      )
    })
  }

  // Maximum number of in-progress expressions.
  const MAX_IN_PROGRESS = 5
  const MAX_RETRIES_IN_QUEUE = 5

  const inProgress = ref(0)
  const queueLength = ref(0)

  function queuedExecuteExpression(
    expressionId: ExternalId,
    expression: string,
    timeoutMs: number = 5000,
  ): Promise<Result<unknown> | null> {
    if (inProgress.value >= MAX_IN_PROGRESS) {
      queueLength.value += 1
      const pause = queueLength.value * 250
      return new Promise((resolve) => setTimeout(resolve, pause)).then(() => {
        queueLength.value -= 1
        return queuedExecuteExpression(expressionId, expression, timeoutMs)
      })
    }

    inProgress.value += 1
    return new Promise<Result<any> | null>((resolve, reject) => {
      const visualizationId = crypto.randomUUID() as Uuid
      let state = 1

      const dataHandler = (visData: VisualizationUpdate, uuid: Uuid | null) => {
        if (uuid !== visualizationId) {
          return
        }

        inProgress.value -= state
        state = 0 // Prevent further updates from this handler.
        dataConnection.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, dataHandler)
        executionContext.off('visualizationEvaluationFailed', errorHandler)
        const dataStr = Ok(visData.dataString())
        const parsed = parseVisualizationData(dataStr)
        resolve(parsed)
      }
      const errorHandler = (
        uuid: Uuid,
        _expressionId: ExpressionId,
        message: string,
        _diagnostic: Diagnostic | undefined,
      ) => {
        if (uuid !== visualizationId) {
          return
        }

        inProgress.value -= state
        state = 0 // Prevent further updates from this handler.
        dataConnection.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, dataHandler)
        executionContext.off('visualizationEvaluationFailed', errorHandler)
        reject(Err(message))
      }

      const waitWithExponentialBackoff = (retryAttempt: number, timeoutMs: number) => {
        wait(timeoutMs).then(() => {
          if (state === 1) {
            if (retryAttempt < MAX_RETRIES_IN_QUEUE) {
              const incRetryAttempt = retryAttempt + 1
              DEV: console.warn(
                'Waiting on data (expressionId=' +
                  expressionId +
                  ', visualizationId=' +
                  visualizationId +
                  '), retry attempt: ' +
                  incRetryAttempt,
              )
              waitWithExponentialBackoff(incRetryAttempt, timeoutMs * 2 ** retryAttempt)
            } else {
              inProgress.value -= 1
              state = 0 // Prevent further updates from this handler.
              dataConnection.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, dataHandler)
              executionContext.off('visualizationEvaluationFailed', errorHandler)
              reject(Err(`executeExpression: Execution timed out.`))
            }
          }
        })
      }
      waitWithExponentialBackoff(0, timeoutMs)

      dataConnection.on(`${OutboundPayload.VISUALIZATION_UPDATE}`, dataHandler)
      executionContext.on('visualizationEvaluationFailed', errorHandler)
      lsRpcConnection.executeExpression(
        executionContext.id,
        visualizationId,
        expressionId,
        expression,
      )
    })
  }

  function parseVisualizationData(data: Result<string | null> | null): Result<unknown> | null {
    if (!data?.ok) return data
    if (data.value == null) return null
    try {
      return Ok(markRaw(JSON.parse(data.value)))
    } catch (error) {
      if (error instanceof SyntaxError)
        return Err(`Parsing visualization result failed: ${error.message}`)
      else throw error
    }
  }

  const { executionMode } = setupSettings(projectModel)

  const recordMode = computed({
    get() {
      return executionMode.value === 'live'
    },
    set(value) {
      executionMode.value = value ? 'live' : 'design'
    },
  })

  watch(executionMode, (modeValue) => {
    executionContext.executionEnvironment = modeValue === 'live' ? 'Live' : 'Design'
  })

  async function renameProject(newDisplayedName: string) {
    try {
      projectNames.onProjectRenameRequested(newDisplayedName)
      const result = await openedProjects.renameProject(projectAssetId, newDisplayedName)
      if (!result.ok) projectNames.onProjectRenameFailed()
      return result
    } catch (err) {
      projectNames.onProjectRenameFailed()
      return Err(err)
    }
  }
  lsRpcConnection.on('refactoring/projectRenamed', ({ oldNormalizedName, newNormalizedName }) => {
    projectNames.onProjectRenamed(oldNormalizedName, newNormalizedName)
  })

  return proxyRefs({
    setObservedFileName(name: string) {
      observedFileName.value = name
    },
    get observedFileName() {
      return observedFileName.value
    },
    id: projectId,
    isOnLocalBackend,
    executionContext,
    firstExecution,
    diagnostics,
    module,
    moduleProjectPath,
    entryPoint,
    projectModel,
    projectRootId,
    awareness: markRaw(awareness),
    computedValueRegistry: markRaw(computedValueRegistry),
    lsRpcConnection: markRaw(lsRpcConnection),
    dataConnection: markRaw(dataConnection),
    useVisualizationData,
    isRecordingEnabled,
    stopCapturingUndo,
    executionMode,
    recordMode,
    dataflowErrors,
    executeExpression,
    queuedExecuteExpression,
    renameProject,
  })
}

function resolveYDocUrl(rpcUrl: string, url: string): URL {
  let resolved
  if (url == '') {
    resolved = new URL(location.origin)
    resolved.protocol = location.protocol.replace(/^http/, 'ws')
  } else if (URL.canParse(url)) {
    resolved = new URL(url)
  } else {
    resolved = new URL(rpcUrl)
    resolved.port = '5976'
  }
  resolved.pathname = '/project'
  return resolved
}

function createLsRpcConnection(clientId: Uuid, url: string, abort: AbortScope): LanguageServer {
  const transport = createRpcTransport(url)
  const connection = new LanguageServer(clientId, transport)
  abort.onAbort(() => {
    connection.stopReconnecting()
    connection.release()
  })
  return connection
}

function initializeDataConnection(clientId: Uuid, url: string, abort: AbortScope) {
  const client = createDataWebsocket(url, 'arraybuffer')
  const connection = new DataServer(clientId, client, abort)
  onScopeDispose(() => connection.dispose())
  return connection
}

type ExecutionMode = 'live' | 'design'
type Settings = { executionMode: WritableComputedRef<ExecutionMode> }
function setupSettings(project: DistributedProject | null): Settings {
  const settings = computed(() => project?.settings)
  // Value synchronized with a key of the `settings` map, used to enforce reactive dependencies.
  const executionMode_ = ref<ExecutionMode>()
  const executionMode = computed<ExecutionMode>({
    get() {
      return executionMode_.value ?? 'design'
    },
    set(value) {
      // Update the synchronized map; the change observer will set `executionMode_`.
      if (settings.value != null) settings.value.set('executionMode', value)
    },
  })
  useObserveYjs(settings, (event) => {
    event.changes.keys.forEach((change, key) => {
      if (key == 'executionMode') {
        if (change.action === 'add' || change.action === 'update') {
          switch (settings.value?.get('executionMode')) {
            case 'design':
              executionMode_.value = 'design'
              break
            case 'live':
              executionMode_.value = 'live'
              break
            default:
              console.log(`Bug: Unexpected executionMode. Ignoring...`, executionMode)
              break
          }
        } else if (change.action === 'delete') {
          executionMode_.value = undefined
        }
      }
    })
  })
  return { executionMode }
}
