import type { ExecutionContext } from '@/stores/project/executionContext'
import { mockProjectNameStore, type ProjectNameStore } from '@/stores/projectNames'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { ANY_TYPE_QN } from '@/util/ensoTypes'
import { parseMethodPointer, type MethodCall } from '@/util/methodPointer'
import { type ProjectPath } from '@/util/projectPath'
import { isQualifiedName } from '@/util/qualifiedName'
import { markRaw } from 'vue'
import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall as LSMethodCall,
  ProfilingInfo,
} from 'ydoc-shared/languageServerTypes'

export interface ExpressionInfo {
  typename: ProjectPath | undefined
  rawTypename: string | undefined
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

  private constructor(private readonly projectNames: ProjectNameStore) {
    markRaw(this)
  }

  /** TODO: Add docs */
  static WithExecutionContext(
    executionContext: ExecutionContext,
    projectNames: ProjectNameStore,
  ): ComputedValueRegistry {
    const self = new ComputedValueRegistry(projectNames)
    self.executionContext = executionContext
    executionContext.on('expressionUpdates', self._updateHandler)
    return self
  }

  /** TODO: Add docs */
  static Mock(projectNames: ProjectNameStore = mockProjectNameStore()): ComputedValueRegistry {
    return new ComputedValueRegistry(projectNames)
  }

  /** TODO: Add docs */
  processUpdates(updates: ExpressionUpdate[]) {
    for (const update of updates) {
      const info = this.db.get(update.expressionId)
      if (info) updateInfo(info, update, this.projectNames)
      else this.db.set(update.expressionId, combineInfo(undefined, update, this.projectNames))
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

function updateInfo(
  info: ExpressionInfo,
  update: ExpressionUpdate,
  projectNames: ProjectNameStore,
) {
  const newInfo = combineInfo(info, update, projectNames)
  if (newInfo.typename !== info.typename) info.typename = newInfo.typename
  if (newInfo.methodCall !== info.methodCall) info.methodCall = newInfo.methodCall
  if (newInfo.payload !== info.payload) info.payload = newInfo.payload
  if (newInfo.profilingInfo !== info.profilingInfo) info.profilingInfo = update.profilingInfo
}

function translateMethodCall(ls: LSMethodCall, projectNames: ProjectNameStore): MethodCall {
  return {
    methodPointer: parseMethodPointer(ls.methodPointer, projectNames),
    notAppliedArguments: ls.notAppliedArguments,
  }
}

function combineInfo(
  info: ExpressionInfo | undefined,
  update: ExpressionUpdate,
  projectNames: ProjectNameStore,
): ExpressionInfo {
  const isPending = update.payload.type === 'Pending'
  const updateSingleValueType = update.type.at(0) // TODO: support multi-value (aka intersection) types
  const rawTypename = updateSingleValueType ?? (isPending ? info?.rawTypename : undefined)
  return {
    typename:
      rawTypename && isQualifiedName(rawTypename) && rawTypename !== ANY_TYPE_QN ?
        projectNames.parseProjectPath(rawTypename)
      : undefined,
    rawTypename,
    methodCall:
      update.methodCall ? translateMethodCall(update.methodCall, projectNames)
      : isPending ? info?.methodCall
      : undefined,
    payload: update.payload,
    profilingInfo: update.profilingInfo,
  }
}
