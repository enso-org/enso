import type { ExecutionContext } from '@/stores/project'
import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
} from 'shared/languageServerTypes'
import { reactive } from 'vue'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
}

/** This class holds the computed values that have been received from the language server. */
export class ComputedValueRegistry {
  private expressionMap: Map<ExpressionId, ExpressionInfo>
  private _updateHandler = this.processUpdates.bind(this)
  private executionContext

  constructor(executionContext: ExecutionContext) {
    this.executionContext = executionContext
    this.expressionMap = reactive(new Map())

    executionContext.on('expressionUpdates', this._updateHandler)
  }

  processUpdates(updates: ExpressionUpdate[]) {
    for (const update of updates) {
      this.expressionMap.set(update.expressionId, {
        typename: update.type,
        methodCall: update.methodCall,
        payload: update.payload,
        profilingInfo: update.profilingInfo,
      })
    }
  }

  getExpressionInfo(exprId: ExpressionId): ExpressionInfo | undefined {
    return this.expressionMap.get(exprId)
  }

  destroy() {
    this.executionContext.off('expressionUpdates', this._updateHandler)
  }
}
