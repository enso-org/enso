import type { ExecutionContext } from '@/stores/project'
import type { VisualizationDataRegistry } from '@/util/visualizationDataRegistry'
import * as random from 'lib0/random'
import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
} from 'shared/languageServerTypes'
import type { Uuid } from 'shared/yjsModel'
import { computed, markRaw, shallowRef } from 'vue'
import { ReactiveDb, ReactiveIndex } from './database/reactiveDb'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
}

class ComputedValueDb extends ReactiveDb<ExpressionId, ExpressionInfo> {
  type = new ReactiveIndex(this, (id, info) => [[id, info.payload.type]])
}

/** This class holds the computed values that have been received from the language server. */
export class ComputedValueRegistry {
  public db = new ComputedValueDb()
  private dataflowErrorVisualizationIds = new Map<ExpressionId, Uuid>()
  private _updateHandler = this.processUpdates.bind(this)
  private executionContext: ExecutionContext | undefined
  private visualizationDataRegistry: VisualizationDataRegistry | undefined

  private constructor() {
    markRaw(this)
  }

  static WithExecutionContext(
    executionContext: ExecutionContext,
    visualizationDataRegistry: VisualizationDataRegistry,
  ): ComputedValueRegistry {
    const self = new ComputedValueRegistry()
    self.executionContext = executionContext
    self.visualizationDataRegistry = visualizationDataRegistry
    executionContext.on('expressionUpdates', self._updateHandler)
    return self
  }

  static Mock(): ComputedValueRegistry {
    return new ComputedValueRegistry()
  }

  processUpdates(updates: ExpressionUpdate[]) {
    for (const update of updates) {
      const info = this.db.get(update.expressionId)
      const newInfo = combineInfo(info, update)
      if (
        (!info || info.payload.type !== 'DataflowError') &&
        newInfo.payload.type === 'DataflowError'
      ) {
        const id = random.uuidv4() as Uuid
        this.dataflowErrorVisualizationIds.set(update.expressionId, id)
        this.executionContext?.setVisualization(id, {
          expressionId: update.expressionId,
          visualizationModule: 'Standard.Visualization.Preprocessor',
          expression: {
            module: 'Standard.Visualization.Preprocessor',
            definedOnType: 'Standard.Visualization.Preprocessor',
            name: 'error_preprocessor',
          },
        })
      } else if (
        info?.payload.type === 'DataflowError' &&
        newInfo.payload.type !== 'DataflowError'
      ) {
        const id = this.dataflowErrorVisualizationIds.get(update.expressionId)
        id && this.executionContext?.setVisualization(id, null)
      }
      this.db.set(update.expressionId, newInfo)
    }
  }

  getExpressionInfo(exprId: ExpressionId): ExpressionInfo | undefined {
    return this.db.get(exprId)
  }

  destroy() {
    this.executionContext?.off('expressionUpdates', this._updateHandler)
  }

  getDataflowError(exprId: ExpressionId) {
    const id = this.dataflowErrorVisualizationIds.get(exprId)
    if (!id) return
    return shallowRef(
      computed<{ kind: 'Dataflow'; message: string } | undefined>(() => {
        const json = this.visualizationDataRegistry?.getRawData(id)
        return json != null ? JSON.parse(json) : undefined
      }),
    )
  }
}

function combineInfo(info: ExpressionInfo | undefined, update: ExpressionUpdate): ExpressionInfo {
  const isPending = update.payload.type === 'Pending'
  return {
    typename: update.type ?? (isPending ? info?.typename : undefined),
    methodCall: update.methodCall ?? (isPending ? info?.methodCall : undefined),
    payload: update.payload,
    profilingInfo: update.profilingInfo,
  }
}
