import { useOpenedProjects } from '$/providers/openedProjects'
import { ComputedValueRegistry } from '$/providers/openedProjects/project/computedValueRegistry'
import {
  ExecutionContext,
  visualizationConfigPreprocessorEqual,
  type NodeVisualizationConfiguration,
} from '$/providers/openedProjects/project/executionContext'
import { VisualizationDataRegistry } from '$/providers/openedProjects/project/visualizationDataRegistry'
import { type ProjectNameStore } from '$/providers/openedProjects/projectNames'
import { proxyRefs } from '$/utils/reactivity'
import { Awareness } from '@/stores/awareness'
import { attachProvider, useObserveYjs } from '@/util/crdt'
import { nextEvent } from '@/util/data/observable'
import type { Opt } from '@/util/data/opt'
import { ReactiveMapping } from '@/util/database/reactiveDb'
import type { MethodPointer } from '@/util/methodPointer'
import { createDataSocket, createRpcTransport, useAbortScope } from '@/util/net'
import { DataServer } from '@/util/net/dataServer'
import { ProjectPath } from '@/util/projectPath'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { ProjectId } from 'enso-common/src/services/Backend'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
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
import { LanguageServer } from 'ydoc-shared/languageServer'
import type { Diagnostic } from 'ydoc-shared/languageServerTypes'
import type { AbortScope } from 'ydoc-shared/util/net'
import {
  newVisRequestId,
  Visualizations,
  VisualizationSlotView,
  type VisRequestId,
  type VisualizationId,
} from 'ydoc-shared/visualizations'
import { DistributedProject, type ExternalId, type Uuid } from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'

export interface LsUrls {
  rpcUrl: string
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
  const ydocUrl = resolveYDocUrl(props.engine.rpcUrl, props.engine.ydocUrl)
  const guiRpcId = `gui-rpc-${crypto.randomUUID()}`
  const clientId = crypto.randomUUID() as Uuid
  const lsRpcConnection = createLsRpcConnection(clientId, doc, guiRpcId, abort)
  const projectRootId = lsRpcConnection.contentRoots.then(
    (roots) => roots.find((root) => root.type === 'Project')?.id,
  )
  onScopeDispose(() => lsRpcConnection.release())

  const guiDataId = `gui-data-${crypto.randomUUID()}`
  const dataConnection = initializeDataConnection(clientId, doc, guiDataId, abort)
  const rpcUrl = new URL(props.engine.rpcUrl)
  const isOnLocalBackend =
    rpcUrl.protocol === 'mock:' ||
    rpcUrl.hostname === 'localhost' ||
    rpcUrl.hostname === '127.0.0.1' ||
    rpcUrl.hostname === '[::1]' ||
    rpcUrl.hostname === '0:0:0:0:0:0:0:1'

  let yDocsProvider: ReturnType<typeof attachProvider> | undefined
  watchEffect((onCleanup) => {
    yDocsProvider = attachProvider(
      ydocUrl.href,
      'index',
      { ls: guiRpcId, data: guiDataId },
      doc,
      awareness.internal,
    )
    onCleanup(() => {
      yDocsProvider?.dispose()
      yDocsProvider = undefined
    })
  })

  const moduleProjectPath = computed((): Result<ProjectPath> | undefined => {
    const filePath = observedFileName.value
    if (filePath == null) return undefined
    const withoutFileExt = filePath.replace(/\.enso$/, '')
    const withDotSeparators = withoutFileExt.replace(/\//g, '.')
    const qn = tryQualifiedName(withDotSeparators)
    if (!qn.ok) return qn
    return Ok(ProjectPath.create(undefined, qn.value))
  })

  const projectModel = new DistributedProject(doc)

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
      projectModel,
    )
  }

  const firstExecution = nextEvent(lsRpcConnection, 'executionContext/executionComplete').catch(
    (error) => {
      console.error('First execution failed:', error)
      throw error
    },
  )
  const executionContext = createExecutionContextForMain()
  const visualizationDataRegistry = new VisualizationDataRegistry(projectModel)
  abort.handleDispose(visualizationDataRegistry)
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

        if (!visualizationConfigPreprocessorEqual(config, oldConfig) || visId.value == null) {
          visId.value = crypto.randomUUID()
        }

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

  /**
   * Evaluate `expression` in the context of node `expressionId`, returning
   * the decoded JSON result.
   *
   * Transport: writes an `execute` slot to the vis subdoc and observes it
   * until it lands in a terminal state (`ready | failed`). Back-pressure is
   * the LS bridge actor's responsibility; retry is a single-shot timeout
   * (caller handles retry if desired, via {@link queuedExecuteExpression}).
   *
   * Returns `null` iff the vis subdoc has not synced yet - the caller can
   * retry momentarily. `Ok(parsed)` on success, `Err(message)` on
   * evaluation failure.
   */
  function executeExpression(
    expressionId: ExternalId,
    expression: string,
  ): Promise<Result<any> | null> {
    return runExecuteExpressionSlot(expressionId, expression, null).then(parseVisualizationData)
  }

  function queuedExecuteExpression(
    expressionId: ExternalId,
    expression: string,
    timeoutMs: number = 5000,
  ): Promise<Result<unknown> | null> {
    return runExecuteExpressionSlot(expressionId, expression, timeoutMs).then(
      parseVisualizationData,
    )
  }

  /**
   * Variant of {@link queuedExecuteExpression} that returns the raw decoded text the LS produced
   * (no JSON parsing). Failure semantics match {@link queuedExecuteExpression}.
   *
   * `onTimeout` fires after the awaiting promise has resolved on timer expiry — callers use it
   * to issue out-of-band cleanup that shouldn't block the resolved Promise. The AI tool handler
   * uses this to send `executionContext/interrupt` so a stuck eval doesn't block subsequent
   * verification calls.
   */
  function queuedExecuteExpressionRaw(
    expressionId: ExternalId,
    expression: string,
    timeoutMs: number = 5000,
    onTimeout?: () => void,
  ): Promise<Result<string> | null> {
    return runExecuteExpressionSlot(expressionId, expression, timeoutMs, onTimeout)
  }

  /**
   * Shared implementation for {@link executeExpression},
   * {@link queuedExecuteExpression}, and {@link queuedExecuteExpressionRaw}. If
   * `timeoutMs` is `null`, wait indefinitely for the slot to terminate;
   * otherwise resolve with `Err(timeout)` once the deadline passes. Either way
   * the slot is removed from the subdoc before the returned promise settles.
   * `onTimeout` is forwarded to {@link awaitExecuteSlot} — see its doc.
   */
  function runExecuteExpressionSlot(
    expressionId: ExternalId,
    expression: string,
    timeoutMs: number | null,
    onTimeout?: () => void,
  ): Promise<Result<string> | null> {
    const visDoc = projectModel.visualizationsDoc
    if (!visDoc) {
      // Subdoc not yet synced. Callers today retry by waiting for UI
      // interactions, so a single-shot `null` return is sufficient.
      console.warn('[executeExpression] vis subdoc not ready; returning null')
      return Promise.resolve(null)
    }
    const vis = new Visualizations(visDoc)
    const visualizationId = crypto.randomUUID() as VisualizationId
    const requestId = newVisRequestId()
    vis.createSlot(
      {
        visualizationId,
        contextId: executionContext.id,
        nodeExternalId: expressionId,
        request: {
          visualizationModule: '',
          expression: { inFrame: expression },
        },
      },
      requestId,
    )
    return awaitExecuteSlot(vis, requestId, timeoutMs, onTimeout).finally(() => {
      vis.removeSlot(requestId)
    })
  }

  /**
   * Observe slot `requestId` until it lands in a terminal state: `ready` → `Ok(text)` (UTF-8
   * decoded), `failed` → `Err(message)`, timeout → `Err(timeout)`. Always resolves; rejection
   * is reserved for catastrophic failures.
   *
   * `onTimeout`, if provided, fires after the awaiting promise has resolved on timer expiry —
   * callers use it to issue out-of-band cleanup (e.g. `executionContext/interrupt`) that
   * shouldn't block the resolved Promise but should run before the next consumer of the
   * execution context.
   */
  function awaitExecuteSlot(
    vis: Visualizations,
    requestId: VisRequestId,
    timeoutMs: number | null,
    onTimeout?: () => void,
  ): Promise<Result<string> | null> {
    return new Promise<Result<string> | null>((resolve) => {
      let unobserve: (() => void) | null = null
      let timer: ReturnType<typeof setTimeout> | null = null

      const finish = (value: Result<string> | null) => {
        if (unobserve) {
          unobserve()
          unobserve = null
        }
        if (timer) {
          clearTimeout(timer)
          timer = null
        }
        resolve(value)
      }

      const commitTerminal = (view: VisualizationSlotView): boolean => {
        if (view.status === 'ready') {
          const bytes = view.response
          if (bytes == null) {
            finish(Err('executeExpression: ready slot with no response bytes'))
            return true
          }
          try {
            const text = new TextDecoder('utf-8', { fatal: false }).decode(bytes)
            finish(Ok(text))
          } catch (e) {
            finish(Err(`Failed to decode visualization response: ${e}`))
          }
          return true
        }
        if (view.status === 'failed') {
          const message = view.failure?.message ?? 'Visualization evaluation failed'
          finish(Err(message))
          return true
        }
        return false
      }

      const existing = vis.getSlot(requestId)
      if (existing && commitTerminal(existing)) return

      const observer = () => {
        const view = vis.getSlot(requestId)
        if (!view) return
        commitTerminal(view)
      }
      vis.slots.observeDeep(observer)
      unobserve = () => vis.slots.unobserveDeep(observer)

      if (timeoutMs != null) {
        timer = setTimeout(() => {
          finish(Err('executeExpression: Execution timed out.'))
          onTimeout?.()
        }, timeoutMs)
      }
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
    executionMode,
    recordMode,
    dataflowErrors,
    executeExpression,
    queuedExecuteExpression,
    queuedExecuteExpressionRaw,
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

function createLsRpcConnection(
  clientId: Uuid,
  doc: Y.Doc,
  url: string,
  abort: AbortScope,
): LanguageServer {
  const transport = createRpcTransport(doc, url)
  const connection = new LanguageServer(clientId, transport)
  abort.onAbort(() => {
    connection.stopReconnecting()
    connection.release()
  })
  return connection
}

function initializeDataConnection(clientId: Uuid, doc: Y.Doc, url: string, abort: AbortScope) {
  const client = createDataSocket(doc, url)
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
