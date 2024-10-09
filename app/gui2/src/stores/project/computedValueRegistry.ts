import type { ExecutionContext } from '@/stores/project/executionContext'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { markRaw } from 'vue'
import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
} from 'ydoc-shared/languageServerTypes'

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
  private _updateHandler = this.processUpdates.bind(this)
  private executionContext: ExecutionContext | undefined

  private constructor() {
    markRaw(this)
  }

  /** TODO: Add docs */
  static WithExecutionContext(executionContext: ExecutionContext): ComputedValueRegistry {
    const self = new ComputedValueRegistry()
    self.executionContext = executionContext
    executionContext.on('expressionUpdates', self._updateHandler)
    return self
  }

  /** TODO: Add docs */
  static Mock(): ComputedValueRegistry {
    return new ComputedValueRegistry()
  }

  /** TODO: Add docs */
  processUpdates(updates: ExpressionUpdate[]) {
    for (const update of updates) {
      const info = this.db.get(update.expressionId)
      if (info) updateInfo(info, update)
      else this.db.set(update.expressionId, combineInfo(undefined, update))
    }
  }

  /** TODO: Add docs */
  getExpressionInfo(exprId: ExpressionId): ExpressionInfo | undefined {
    return this.db.get(exprId)
  }

  /** TODO: Add docs */
  dispose() {
    this.executionContext?.off('expressionUpdates', this._updateHandler)
  }
}

function updateInfo(info: ExpressionInfo, update: ExpressionUpdate) {
  const newInfo = combineInfo(info, update)
  if (newInfo.typename !== info.typename) info.typename = newInfo.typename
  if (newInfo.methodCall !== info.methodCall) info.methodCall = newInfo.methodCall
  if (newInfo.payload !== info.payload) info.payload = newInfo.payload
  if (newInfo.profilingInfo !== info.profilingInfo) info.profilingInfo = update.profilingInfo
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
