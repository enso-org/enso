import type { ExecutionContext } from '@/stores/project'
import { ObservableV2 } from 'lib0/observable'
import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
} from 'shared/languageServerTypes'
import { markRaw } from 'vue'
import { ReactiveDb, ReactiveIndex } from './database/reactiveDb'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
}

type ComputedValueRegistryEvents = {
  update: (
    update: ExpressionUpdate,
    oldInfo: ExpressionInfo | undefined,
    newInfo: ExpressionInfo,
  ) => void
}

class ComputedValueDb extends ReactiveDb<ExpressionId, ExpressionInfo> {
  type = new ReactiveIndex(this, (id, info) => [[id, info.payload.type]])
}

/** This class holds the computed values that have been received from the language server. */
export class ComputedValueRegistry extends ObservableV2<ComputedValueRegistryEvents> {
  public db = new ComputedValueDb()
  private _updateHandler = this.processUpdates.bind(this)
  private executionContext: ExecutionContext | undefined

  private constructor() {
    super()
    markRaw(this)
  }

  static WithExecutionContext(executionContext: ExecutionContext): ComputedValueRegistry {
    const self = new ComputedValueRegistry()
    self.executionContext = executionContext
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
      this.emit('update', [update, info, newInfo])
      this.db.set(update.expressionId, newInfo)
    }
  }

  getExpressionInfo(exprId: ExpressionId): ExpressionInfo | undefined {
    return this.db.get(exprId)
  }

  destroy() {
    this.executionContext?.off('expressionUpdates', this._updateHandler)
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
