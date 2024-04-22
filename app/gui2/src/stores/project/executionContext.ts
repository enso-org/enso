import { isSome, type Opt } from '@/util/data/opt'
import { Err, Ok, type Result } from '@/util/data/result'
import { AsyncQueue, type AbortScope } from '@/util/net'
import * as array from 'lib0/array'
import * as object from 'lib0/object'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import type { LanguageServer } from 'shared/languageServer'
import type {
  ContextId,
  Diagnostic,
  ExecutionEnvironment,
  ExplicitCall,
  ExpressionId,
  ExpressionUpdate,
  StackItem,
  Uuid,
  VisualizationConfiguration,
} from 'shared/languageServerTypes'
import { exponentialBackoff } from 'shared/util/net'
import type { ExternalId } from 'shared/yjsModel'
import { reactive } from 'vue'

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

interface ExecutionContextState {
  lsRpc: LanguageServer
  created: boolean
  visualizations: Map<Uuid, NodeVisualizationConfiguration>
  stack: StackItem[]
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
  id: ContextId = random.uuidv4() as ContextId
  queue: AsyncQueue<ExecutionContextState>
  taskRunning = false
  visSyncScheduled = false
  desiredStack: StackItem[] = reactive([])
  visualizationConfigs: Map<Uuid, NodeVisualizationConfiguration> = new Map()

  constructor(
    lsRpc: LanguageServer,
    entryPoint: EntryPoint,
    private abort: AbortScope,
  ) {
    super()
    this.abort.handleDispose(this)

    this.queue = new AsyncQueue<ExecutionContextState>(
      Promise.resolve({
        lsRpc,
        created: false,
        visualizations: new Map(),
        stack: [],
      }),
    )
    this.registerHandlers()
    this.create()
    this.pushItem({ type: 'ExplicitCall', ...entryPoint })
    this.recompute()
  }

  private async withBackoff<T>(f: () => Promise<Result<T>>, message: string): Promise<T> {
    const result = await exponentialBackoff(f, {
      onBeforeRetry: (error, _, delay) => {
        if (this.abort.signal.aborted) return false
        console.warn(`${error.message(message)}. Retrying after ${delay}ms...\n`)
      },
    })
    if (result.ok) return result.value
    else throw result.error
  }

  private syncVisualizations() {
    if (this.visSyncScheduled || this.abort.signal.aborted) return
    this.visSyncScheduled = true
    this.queue.pushTask(async (state) => {
      this.visSyncScheduled = false
      if (!state.created || this.abort.signal.aborted) return state
      this.emit('newVisualizationConfiguration', [new Set(this.visualizationConfigs.keys())])
      const promises: Promise<void>[] = []

      const attach = (id: Uuid, config: NodeVisualizationConfiguration) => {
        return this.withBackoff(
          () =>
            state.lsRpc.attachVisualization(id, config.expressionId, {
              executionContextId: this.id,
              expression: config.expression,
              visualizationModule: config.visualizationModule,
              ...(config.positionalArgumentsExpressions ?
                { positionalArgumentsExpressions: config.positionalArgumentsExpressions }
              : {}),
            }),
          'Failed to attach visualization',
        ).then(() => {
          state.visualizations.set(id, config)
        })
      }

      const modify = (id: Uuid, config: NodeVisualizationConfiguration) => {
        return this.withBackoff(
          () =>
            state.lsRpc.modifyVisualization(id, {
              executionContextId: this.id,
              expression: config.expression,
              visualizationModule: config.visualizationModule,
              ...(config.positionalArgumentsExpressions ?
                { positionalArgumentsExpressions: config.positionalArgumentsExpressions }
              : {}),
            }),
          'Failed to modify visualization',
        ).then(() => {
          state.visualizations.set(id, config)
        })
      }

      const detach = (id: Uuid, config: NodeVisualizationConfiguration) => {
        return this.withBackoff(
          () => state.lsRpc.detachVisualization(id, config.expressionId, this.id),
          'Failed to detach visualization',
        ).then(() => {
          state.visualizations.delete(id)
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

      this.emit('visualizationsConfigured', [new Set(this.visualizationConfigs.keys())])

      // State object was updated in-place in each successful promise.
      return state
    })
  }

  private pushItem(item: StackItem) {
    this.desiredStack.push(item)
    this.queue.pushTask(async (state) => {
      if (!state.created) return state
      await this.withBackoff(
        () => state.lsRpc.pushExecutionContextItem(this.id, item),
        'Failed to push item to execution context stack',
      )
      state.stack.push(item)
      return state
    })
  }

  push(expressionId: ExpressionId) {
    this.pushItem({ type: 'LocalCall', expressionId })
  }

  pop() {
    if (this.desiredStack.length === 1) {
      console.debug('Cannot pop last item from execution context stack')
      return
    }
    this.desiredStack.pop()
    this.queue.pushTask(async (state) => {
      if (!state.created) return state
      if (state.stack.length === 1) {
        console.debug('Cannot pop last item from execution context stack')
        return state
      }
      await this.withBackoff(
        () => state.lsRpc.popExecutionContextItem(this.id),
        'Failed to pop item from execution context stack',
      )
      state.stack.pop()
      return state
    })
  }

  async setVisualization(id: Uuid, configuration: Opt<NodeVisualizationConfiguration>) {
    if (configuration == null) {
      this.visualizationConfigs.delete(id)
    } else {
      this.visualizationConfigs.set(id, configuration)
    }
    this.syncVisualizations()
  }

  private create() {
    this.queue.pushTask(async (state) => {
      if (state.created) return state
      return this.withBackoff(async () => {
        const result = await state.lsRpc.createExecutionContext(this.id)
        if (!result.ok) return result
        if (result.value.contextId !== this.id) {
          return Err('Unexpected Context ID returned by the language server.')
        }
        state.lsRpc.retain()
        return Ok({ ...state, created: true })
      }, 'Failed to create execution context')
    })
  }

  private registerHandlers() {
    this.queue.pushTask(async (state) => {
      this.abort.handleObserve(state.lsRpc, 'executionContext/expressionUpdates', (event) => {
        if (event.contextId == this.id) this.emit('expressionUpdates', [event.updates])
      })
      this.abort.handleObserve(state.lsRpc, 'executionContext/executionFailed', (event) => {
        if (event.contextId == this.id) this.emit('executionFailed', [event.message])
      })
      this.abort.handleObserve(state.lsRpc, 'executionContext/executionComplete', (event) => {
        if (event.contextId == this.id) this.emit('executionComplete', [])
      })
      this.abort.handleObserve(state.lsRpc, 'executionContext/executionStatus', (event) => {
        if (event.contextId == this.id) this.emit('executionStatus', [event.diagnostics])
      })
      this.abort.handleObserve(
        state.lsRpc,
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
      return state
    })
  }

  recompute(
    expressionIds: 'all' | ExternalId[] = 'all',
    executionEnvironment?: ExecutionEnvironment,
  ) {
    this.queue.pushTask(async (state) => {
      if (!state.created) return state
      await state.lsRpc.recomputeExecutionContext(this.id, expressionIds, executionEnvironment)
      return state
    })
  }

  getStackBottom(): StackItem {
    return this.desiredStack[0]!
  }

  getStackTop(): StackItem {
    return this.desiredStack[this.desiredStack.length - 1]!
  }

  setExecutionEnvironment(mode: ExecutionEnvironment) {
    this.queue.pushTask(async (state) => {
      await state.lsRpc.setExecutionEnvironment(this.id, mode)
      return state
    })
  }

  dispose() {
    this.queue.pushTask(async (state) => {
      if (!state.created) return state
      const result = await state.lsRpc.destroyExecutionContext(this.id)
      if (!result.ok) {
        result.error.log('Failed to destroy execution context')
      }
      state.lsRpc.release()
      return { ...state, created: false }
    })
  }
}
