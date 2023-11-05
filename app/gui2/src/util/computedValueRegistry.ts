import type { ExecutionContext } from '@/stores/project'
import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
} from 'shared/languageServerTypes'
import { markRaw } from 'vue'
import { ReactiveDb } from './database/reactiveDb'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
}

/** This class holds the computed values that have been received from the language server. */
export class ComputedValueRegistry {
  public db: ReactiveDb<ExpressionId, ExpressionInfo>
  private _updateHandler = this.processUpdates.bind(this)
  private executionContext

  constructor(executionContext: ExecutionContext) {
    markRaw(this)
    this.executionContext = executionContext
    this.db = new ReactiveDb()

    executionContext.on('expressionUpdates', this._updateHandler)
  }

  processUpdates(updates: ExpressionUpdate[]) {
    for (const update of updates) {
      this.db.set(update.expressionId, {
        typename: update.type,
        methodCall: update.methodCall,
        payload: update.payload,
        profilingInfo: update.profilingInfo,
      })
    }
  }

  getExpressionInfo(exprId: ExpressionId): ExpressionInfo | undefined {
    return this.db.get(exprId)
  }

  destroy() {
    this.executionContext.off('expressionUpdates', this._updateHandler)
  }
}
