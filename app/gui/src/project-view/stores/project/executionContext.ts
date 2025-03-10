import { type ProjectNameStore } from '@/stores/projectNames'
import { assert } from '@/util/assert'
import { findDifferenceIndex } from '@/util/data/array'
import { isSome, type Opt } from '@/util/data/opt'
import { Err, Ok, ResultError, type Result } from '@/util/data/result'
import {
  methodPointerEquals,
  stackItemsEqual,
  type ExplicitCall,
  type MethodPointer,
  type StackItem,
} from '@/util/methodPointer'
import { AsyncQueue, type AbortScope } from '@/util/net'
import * as array from 'lib0/array'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import { reactive } from 'vue'
import {
  ErrorCode,
  LsRpcError,
  RemoteRpcError,
  type LanguageServer,
} from 'ydoc-shared/languageServer'
import type {
  ContextId,
  Diagnostic,
  ExecutionEnvironment,
  ExpressionId,
  ExpressionUpdate,
  LSMethodPointer,
  StackItem as LSStackItem,
  VisualizationConfiguration as LSVisualizationConfiguration,
  Uuid,
} from 'ydoc-shared/languageServerTypes'
import { exponentialBackoff } from 'ydoc-shared/util/net'
import type { ExternalId } from 'ydoc-shared/yjsModel'

// This constant should be synchronized with EXECUTION_ENVIRONMENT constant in
// engine/runtime/src/main/java/org/enso/interpreter/EnsoLanguage.java
const DEFAULT_ENVIRONMENT: ExecutionEnvironment = 'Design'

export type VisualizationConfiguration = Omit<LSVisualizationConfiguration, 'expression'> & {
  /** An expression that creates a visualization. */
  expression: string | MethodPointer
}

export type NodeVisualizationConfiguration = Omit<
  VisualizationConfiguration,
  'executionContextId'
> & {
  expressionId: ExternalId
}

function visualizationConfigEqual(
  a: NodeVisualizationConfiguration,
  b: NodeVisualizationConfiguration,
): boolean {
  return (
    visualizationConfigPreprocessorEqual(a, b) &&
    (a.positionalArgumentsExpressions === b.positionalArgumentsExpressions ||
      (Array.isArray(a.positionalArgumentsExpressions) &&
        Array.isArray(b.positionalArgumentsExpressions) &&
        array.equalFlat(a.positionalArgumentsExpressions, b.positionalArgumentsExpressions)))
  )
}

/** Same as {@link visualizationConfigEqual}, but ignores differences in {@link NodeVisualizationConfiguration.positionalArgumentsExpressions}. */
export function visualizationConfigPreprocessorEqual(
  a: NodeVisualizationConfiguration,
  b: NodeVisualizationConfiguration,
): boolean {
  return (
    a == b ||
    (a.visualizationModule === b.visualizationModule &&
      (a.expression === b.expression ||
        (typeof a.expression === 'object' &&
          typeof b.expression === 'object' &&
          methodPointerEquals(a.expression, b.expression))))
  )
}

type ExecutionContextState =
  | { status: 'not-created' }
  | {
      status: 'created'
      visualizations: Map<Uuid, NodeVisualizationConfiguration>
      stack: StackItem[]
      environment: ExecutionEnvironment
    }

type EntryPoint = Omit<ExplicitCall, 'type'>

type ExecutionContextNotification = {
  'expressionUpdates'(updates: ExpressionUpdate[]): void
  'visualizationEvaluationFailed'(
    visualizationId: Uuid,
    expressionId: ExpressionId,
    message: string,
    diagnostic: Diagnostic | undefined,
  ): void
  'executionFailed'(message: string): void
  'executionComplete'(): void
  'executionStatus'(diagnostics: Diagnostic[]): void
  'newVisualizationConfiguration'(configs: Set<Uuid>): void
  'visualizationsConfigured'(configs: Set<Uuid>): void
}

enum SyncStatus {
  NOT_SYNCED,
  QUEUED,
  CREATING,
  SYNCING,
  SYNCED,
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
export class ExecutionContext extends ObservableV2<ExecutionContextNotification> {
  readonly id: ContextId = random.uuidv4() as ContextId
  private queue: AsyncQueue<ExecutionContextState>
  private syncStatus = SyncStatus.NOT_SYNCED
  private clearScheduled = false
  private _desiredStack: StackItem[] = reactive([])
  private visualizationConfigs: Map<Uuid, NodeVisualizationConfiguration> = new Map()
  private _executionEnvironment: ExecutionEnvironment = 'Design'

  /** TODO: Add docs */
  constructor(
    private lsRpc: LanguageServer,
    entryPoint: EntryPoint,
    private abort: AbortScope,
    private readonly projectNames: ProjectNameStore,
  ) {
    super()
    this.abort.handleDispose(this)
    this.lsRpc.retain()
    this.queue = new AsyncQueue<ExecutionContextState>(Promise.resolve({ status: 'not-created' }))
    this.registerHandlers()
    this.pushItem({ type: 'ExplicitCall', ...entryPoint })
  }

  private registerHandlers() {
    this.abort.handleObserve(this.lsRpc, 'executionContext/expressionUpdates', (event) => {
      if (event.contextId == this.id) this.emit('expressionUpdates', [event.updates])
    })
    this.abort.handleObserve(this.lsRpc, 'executionContext/executionFailed', (event) => {
      if (event.contextId == this.id) this.emit('executionFailed', [event.message])
    })
    this.abort.handleObserve(this.lsRpc, 'executionContext/executionComplete', (event) => {
      if (event.contextId == this.id) this.emit('executionComplete', [])
    })
    this.abort.handleObserve(this.lsRpc, 'executionContext/executionStatus', (event) => {
      if (event.contextId == this.id) this.emit('executionStatus', [event.diagnostics])
    })
    this.abort.handleObserve(
      this.lsRpc,
      'executionContext/visualizationEvaluationFailed',
      (event) => {
        if (event.contextId == this.id)
          this.emit('visualizationEvaluationFailed', [
            event.visualizationId,
            event.expressionId,
            event.message,
            event.diagnostic,
          ])
      },
    )
    this.lsRpc.on('transport/closed', () => {
      // Connection closed: the created execution context is no longer available
      // There is no point in any scheduled action until resynchronization
      this.queue.clear()
      // If syncing is at the first step (creating missing execution context), it is
      // effectively waiting for reconnection to recreate the execution context.
      // We should not clear it's outcome, as it's likely the context will be created after
      // reconnection (so it's valid).
      if (this.syncStatus !== SyncStatus.CREATING) {
        // In other cases, any created context is destroyed after losing connection.
        // The status should be cleared to 'not-created'.
        this.queue.pushTask(() => {
          this.clearScheduled = false
          this.sync()
          return Promise.resolve({ status: 'not-created' })
        })
        this.clearScheduled = true
      }
    })
  }

  private pushItem(item: StackItem) {
    this._desiredStack.push(item)
    this.sync()
  }

  /**
   * The stack of execution frames that we want to currently inspect. The actual stack
   * state in the language server can differ, since it is updated asynchronously.
   */
  get desiredStack() {
    return this._desiredStack
  }

  /**
   * Set the currently desired stack of excution frames. This will cause appropriate
   * stack push/pop operations to be sent to the language server.
   */
  set desiredStack(stack: StackItem[]) {
    this._desiredStack.length = 0
    this._desiredStack.push(...stack)
    this.sync()
  }

  /** TODO: Add docs */
  push(expressionId: ExpressionId) {
    this.pushItem({ type: 'LocalCall', expressionId })
  }

  /** TODO: Add docs */
  pop() {
    if (this._desiredStack.length === 1) {
      console.debug('Cannot pop last item from execution context stack')
      return
    }
    this._desiredStack.pop()
    this.sync()
  }

  /** TODO: Add docs */
  setVisualization(id: Uuid, configuration: Opt<NodeVisualizationConfiguration>) {
    if (configuration == null) {
      this.visualizationConfigs.delete(id)
    } else {
      this.visualizationConfigs.set(id, configuration)
    }
    this.sync()
  }

  /** See {@link LanguageServer.recomputeExecutionContext}. */
  recompute(
    invalidatedIds?: 'all' | ExternalId[],
    executionEnvironment?: ExecutionEnvironment,
    expressionConfigs?: {
      expressionId: ExpressionId
      executionEnvironment?: ExecutionEnvironment
    }[],
  ) {
    this.queue.pushTask(async (state) => {
      if (state.status !== 'created') {
        this.sync()
      }
      await this.lsRpc.recomputeExecutionContext(
        this.id,
        invalidatedIds,
        executionEnvironment,
        expressionConfigs,
      )
      return state
    })
  }

  /** TODO: Add docs */
  getStackBottom(): StackItem {
    return this._desiredStack[0]!
  }

  /** TODO: Add docs */
  getStackTop(): StackItem {
    return this._desiredStack[this._desiredStack.length - 1]!
  }

  /** TODO: Add docs */
  get executionEnvironment() {
    return this._executionEnvironment
  }

  /** TODO: Add docs */
  set executionEnvironment(env: ExecutionEnvironment) {
    this._executionEnvironment = env
    this.sync()
  }

  /** TODO: Add docs */
  dispose() {
    this.queue.pushTask(async (state) => {
      if (state.status === 'created') {
        const result = await this.withBackoff(
          () => this.lsRpc.destroyExecutionContext(this.id),
          'Destroying execution context',
        )
        if (!result.ok && !this.lsRpc.isDisposed) {
          result.error.log('Failed to destroy execution context')
        }
      }
      if (!this.lsRpc.isDisposed) {
        this.lsRpc.release()
      }
      return { status: 'not-created' }
    })
  }

  private sync() {
    if (
      this.syncStatus === SyncStatus.QUEUED ||
      this.syncStatus === SyncStatus.CREATING ||
      this.abort.signal.aborted
    )
      return
    this.syncStatus = SyncStatus.QUEUED
    this.queue.pushTask(this.syncTask())
  }

  private withBackoff<T>(f: () => Promise<Result<T>>, message: string): Promise<Result<T>> {
    return exponentialBackoff(f, {
      onBeforeRetry: (error, _, delay) => {
        if (this.abort.signal.aborted || this.clearScheduled) return false
        console.warn(`${error.message(message)}. Retrying after ${delay}ms...\n`)
      },
      onFailure(error) {
        error.log(message)
      },
    })
  }

  private syncTask() {
    return async (state: ExecutionContextState) => {
      let newState = { ...state }

      const ensureCreated = () => {
        if (newState.status === 'created') return Ok()
        return this.withBackoff(async () => {
          const result = await this.lsRpc.createExecutionContext(this.id)
          if (!result.ok) return result
          if (result.value.contextId !== this.id) {
            return Err('Unexpected Context ID returned by the language server.')
          }
          newState = {
            status: 'created',
            visualizations: new Map(),
            stack: [],
            environment: DEFAULT_ENVIRONMENT,
          }
          return Ok()
        }, 'Failed to create execution context')
      }

      const syncEnvironment = async () => {
        const state = newState
        if (state.status !== 'created')
          return Err('Cannot sync execution environment when context is not created')
        if (state.environment === this._executionEnvironment) return Ok()
        const result = await this.lsRpc.setExecutionEnvironment(this.id, this._executionEnvironment)
        if (!result.ok) return result
        state.environment = this._executionEnvironment
        return Ok()
      }

      const syncStack = async () => {
        const state = newState
        if (state.status !== 'created')
          return Err('Cannot sync stack when execution context is not created')
        while (true) {
          // Since this is an async function, the desired state can change inbetween individual API calls.
          // We need to compare the desired stack state against current state on every loop iteration.

          const firstDifferent = findDifferenceIndex(
            this._desiredStack,
            state.stack,
            stackItemsEqual,
          )

          if (state.stack.length > firstDifferent) {
            // Found a difference within currently set stack context. We need to pop our way up to it.
            const popResult = await this.withBackoff(
              () => this.lsRpc.popExecutionContextItem(this.id),
              'Failed to pop execution stack frame',
            )
            if (popResult.ok) state.stack.pop()
            else return popResult
          } else if (state.stack.length < this._desiredStack.length) {
            // Desired stack is matching current state, but it is longer. We need to push the next item.
            const newItem = this._desiredStack[state.stack.length]!
            const pushResult = await this.withBackoff(
              () =>
                this.lsRpc.pushExecutionContextItem(
                  this.id,
                  serializeStackItem(newItem, this.projectNames),
                ),
              'Failed to push execution stack frame',
            )
            if (pushResult.ok) state.stack.push(newItem)
            else return pushResult
          } else break
        }

        return Ok()
      }

      const syncVisualizations = async () => {
        const state = newState
        if (state.status !== 'created')
          return Err('Cannot sync visualizations when execution context is not created')
        const promises: Promise<void>[] = []

        const attach = (id: Uuid, config: NodeVisualizationConfiguration) => {
          return this.withBackoff(
            () =>
              this.lsRpc.attachVisualization(id, config.expressionId, {
                executionContextId: this.id,
                expression:
                  typeof config.expression === 'string' ?
                    config.expression
                  : serializeMethodPointer(config.expression, this.projectNames),
                visualizationModule: config.visualizationModule,
                ...(config.positionalArgumentsExpressions ?
                  { positionalArgumentsExpressions: config.positionalArgumentsExpressions }
                : {}),
              }),
            'Failed to attach visualization',
          ).then((result) => {
            if (result.ok) state.visualizations.set(id, config)
          })
        }

        const modify = (id: Uuid, config: NodeVisualizationConfiguration) => {
          return this.withBackoff(
            () =>
              this.lsRpc.modifyVisualization(id, {
                executionContextId: this.id,
                expression:
                  typeof config.expression === 'string' ?
                    config.expression
                  : serializeMethodPointer(config.expression, this.projectNames),
                visualizationModule: config.visualizationModule,
                ...(config.positionalArgumentsExpressions ?
                  { positionalArgumentsExpressions: config.positionalArgumentsExpressions }
                : {}),
              }),
            'Failed to modify visualization',
          ).then((result) => {
            if (result.ok) state.visualizations.set(id, config)
          })
        }

        const detach = (id: Uuid, config: NodeVisualizationConfiguration) => {
          return this.withBackoff(
            () => this.lsRpc.detachVisualization(id, config.expressionId, this.id),
            'Failed to detach visualization',
          ).then((result) => {
            if (result.ok) state.visualizations.delete(id)
          })
        }

        // Attach new and update existing visualizations.
        for (const [id, config] of this.visualizationConfigs) {
          const previousConfig = state.visualizations.get(id)
          if (previousConfig == null) {
            promises.push(attach(id, config))
          } else if (!visualizationConfigEqual(previousConfig, config)) {
            if (previousConfig.expressionId === config.expressionId) {
              promises.push(modify(id, config))
            } else {
              promises.push(detach(id, previousConfig).then(() => attach(id, config)))
            }
          }
        }

        // Detach removed visualizations.
        for (const [id, config] of state.visualizations) {
          if (!this.visualizationConfigs.get(id)) {
            promises.push(detach(id, config))
          }
        }
        const settled = await Promise.allSettled(promises)

        // Emit errors for failed requests.
        const errors = settled
          .map((result) => (result.status === 'rejected' ? result.reason : null))
          .filter(isSome)
        if (errors.length > 0) {
          const result = Err(`Failed to synchronize visualizations: ${errors}`)
          result.error.log()
          return result
        }
        return Ok()
      }

      const handleError = (error: ResultError): ExecutionContextState => {
        // If error tells us that the execution context is missing, we schedule
        // another sync to re-create it, and set proper state.
        if (
          error.payload instanceof LsRpcError &&
          error.payload.cause instanceof RemoteRpcError &&
          error.payload.cause.code === ErrorCode.CONTEXT_NOT_FOUND
        ) {
          this.sync()
          return { status: 'not-created' }
        } else {
          return newState
        }
      }

      this.syncStatus = SyncStatus.CREATING
      try {
        if (this.abort.signal.aborted) return newState
        const createResult = await ensureCreated()
        if (!createResult.ok) return newState

        DEV: assert(this.syncStatus === SyncStatus.CREATING)
        this.syncStatus = SyncStatus.SYNCING

        const syncStackResult = await syncStack()
        if (!syncStackResult.ok) return handleError(syncStackResult.error)
        if (this.syncStatus !== SyncStatus.SYNCING || this.clearScheduled) return newState

        const syncEnvResult = await syncEnvironment()
        if (!syncEnvResult.ok) return handleError(syncEnvResult.error)
        if (this.syncStatus !== SyncStatus.SYNCING || this.clearScheduled) return newState

        this.emit('newVisualizationConfiguration', [new Set(this.visualizationConfigs.keys())])
        const syncVisResult = await syncVisualizations()
        this.emit('visualizationsConfigured', [
          new Set(state.status === 'created' ? state.visualizations.keys() : []),
        ])
        if (!syncVisResult.ok) return handleError(syncVisResult.error)
        if (this.syncStatus !== SyncStatus.SYNCING || this.clearScheduled) return newState

        this.syncStatus = SyncStatus.SYNCED
        return newState
      } finally {
        // On any exception or early return we assme we're not fully synced.
        if (this.syncStatus === SyncStatus.SYNCING || this.syncStatus === SyncStatus.CREATING) {
          this.syncStatus = SyncStatus.NOT_SYNCED
        }
      }
    }
  }
}

function serializeStackItem(stackItem: StackItem, projectNames: ProjectNameStore): LSStackItem {
  return stackItem.type === 'ExplicitCall' ?
      {
        ...stackItem,
        methodPointer: serializeMethodPointer(stackItem.methodPointer, projectNames),
      }
    : stackItem
}

function serializeMethodPointer(
  methodPointer: MethodPointer,
  projectNames: ProjectNameStore,
): LSMethodPointer {
  return {
    module: projectNames.serializeProjectPathForBackend(methodPointer.module),
    definedOnType: projectNames.serializeProjectPathForBackend(methodPointer.definedOnType),
    name: methodPointer.name,
  }
}
