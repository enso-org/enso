import { findIndexOpt } from '@/util/data/array'
import { isSome, type Opt } from '@/util/data/opt'
import { Err, Ok, type Result } from '@/util/data/result'
import { AsyncQueue, type AbortScope } from '@/util/net'
import {
  qnReplaceProjectName,
  tryIdentifier,
  tryQualifiedName,
  type Identifier,
} from '@/util/qualifiedName'
import * as array from 'lib0/array'
import * as object from 'lib0/object'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import { reactive } from 'vue'
import type { LanguageServer } from 'ydoc-shared/languageServer'
import {
  stackItemsEqual,
  type ContextId,
  type Diagnostic,
  type ExecutionEnvironment,
  type ExplicitCall,
  type ExpressionId,
  type ExpressionUpdate,
  type StackItem,
  type Uuid,
  type VisualizationConfiguration,
} from 'ydoc-shared/languageServerTypes'
import { exponentialBackoff } from 'ydoc-shared/util/net'
import type { ExternalId } from 'ydoc-shared/yjsModel'

// This constant should be synchronized with EXECUTION_ENVIRONMENT constant in
// engine/runtime/src/main/java/org/enso/interpreter/EnsoLanguage.java
const DEFAULT_ENVIRONMENT: ExecutionEnvironment = 'Design'

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
    a === b ||
    (a.visualizationModule === b.visualizationModule &&
      (a.positionalArgumentsExpressions === b.positionalArgumentsExpressions ||
        (Array.isArray(a.positionalArgumentsExpressions) &&
          Array.isArray(b.positionalArgumentsExpressions) &&
          array.equalFlat(a.positionalArgumentsExpressions, b.positionalArgumentsExpressions))) &&
      (a.expression === b.expression ||
        (typeof a.expression === 'object' &&
          typeof b.expression === 'object' &&
          object.equalFlat(a.expression, b.expression))))
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
      this.syncStatus = SyncStatus.NOT_SYNCED
      this.queue.pushTask(() => {
        this.clearScheduled = false
        this.sync()
        return Promise.resolve({ status: 'not-created' })
      })
      this.clearScheduled = true
    })
    this.lsRpc.on('refactoring/projectRenamed', ({ oldNormalizedName, newNormalizedName }) => {
      const newIdent = tryIdentifier(newNormalizedName)
      if (!newIdent.ok) {
        console.error(
          `Cannot update project name in execution stack: new name ${newNormalizedName} is not a valid identifier!`,
        )
        return
      }
      ExecutionContext.replaceProjectNameInStack(
        this._desiredStack,
        oldNormalizedName,
        newIdent.value,
      )
      if (this.syncStatus === SyncStatus.SYNCED) {
        this.queue.pushTask((state) => {
          if (state.status !== 'created') return Promise.resolve(state)
          ExecutionContext.replaceProjectNameInStack(state.stack, oldNormalizedName, newIdent.value)
          return Promise.resolve(state)
        })
      } else {
        // Engine updates project name in its execution context frames by itself. But if we are out
        // of sync, we have no guarantee if the stack wasn't set with old name after project rename.
        // It's safer to just re-sync the stack.
        this.sync()
      }
    })
  }

  private pushItem(item: StackItem) {
    this._desiredStack.push(item)
    this.sync()
  }

  private static replaceProjectNameInStack(
    stack: StackItem[],
    oldName: string,
    newName: Identifier,
  ) {
    const updatedField = (value: string) => {
      const qn = tryQualifiedName(value)
      if (qn.ok) {
        return qnReplaceProjectName(qn.value, oldName, newName)
      } else {
        console.warn(`Invalid qualified name in execution context stack: ${value}`)
        return value
      }
    }
    for (const item of stack) {
      if (item.type === 'ExplicitCall') {
        item.methodPointer.module = updatedField(item.methodPointer.module)
        item.methodPointer.definedOnType = updatedField(item.methodPointer.definedOnType)
      }
    }
  }

  /** TODO: Add docs */
  get desiredStack() {
    return this._desiredStack
  }

  /** TODO: Add docs */
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

  /** TODO: Add docs */
  recompute(
    expressionIds: 'all' | ExternalId[] = 'all',
    executionEnvironment?: ExecutionEnvironment,
  ) {
    this.queue.pushTask(async (state) => {
      if (state.status !== 'created') {
        this.sync()
      }
      await this.lsRpc.recomputeExecutionContext(this.id, expressionIds, executionEnvironment)
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
    if (this.syncStatus === SyncStatus.QUEUED || this.abort.signal.aborted) return
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
      this.syncStatus = SyncStatus.SYNCING
      if (this.abort.signal.aborted) return state
      let newState = { ...state }

      const create = () => {
        if (newState.status === 'created') return Ok()
        // if (newState.status === 'broken') {
        //   this.withBackoff(() => this.lsRpc.destroyExecutionContext(this.id), 'Failed to destroy broken execution context')
        // }
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
        const firstDifferent =
          findIndexOpt(this._desiredStack, (item, index) => {
            const stateStack = state.stack[index]
            return stateStack == null || !stackItemsEqual(item, stateStack)
          }) ?? this._desiredStack.length
        for (let i = state.stack.length; i > firstDifferent; --i) {
          const popResult = await this.withBackoff(
            () => this.lsRpc.popExecutionContextItem(this.id),
            'Failed to pop execution stack frame',
          )
          if (popResult.ok) state.stack.pop()
          else return popResult
        }
        for (let i = state.stack.length; i < this._desiredStack.length; ++i) {
          const newItem = this._desiredStack[i]!
          const pushResult = await this.withBackoff(
            () => this.lsRpc.pushExecutionContextItem(this.id, newItem),
            'Failed to push execution stack frame',
          )
          if (pushResult.ok) state.stack.push(newItem)
          else return pushResult
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
                expression: config.expression,
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
                expression: config.expression,
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
          console.error('Failed to synchronize visualizations:', errors)
        }
      }

      const createResult = await create()
      if (!createResult.ok) return newState
      const syncStackResult = await syncStack()
      if (!syncStackResult.ok) return newState
      const syncEnvResult = await syncEnvironment()
      if (!syncEnvResult.ok) return newState
      this.emit('newVisualizationConfiguration', [new Set(this.visualizationConfigs.keys())])
      await syncVisualizations()
      this.emit('visualizationsConfigured', [
        new Set(state.status === 'created' ? state.visualizations.keys() : []),
      ])
      if (this.syncStatus === SyncStatus.SYNCING) {
        this.syncStatus = SyncStatus.SYNCED
      }
      return newState
    }
  }
}
