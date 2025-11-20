import type { ExecutionContext } from '$/providers/openedProjects/project/executionContext'
import {
  mockProjectNameStore,
  type ProjectNameStore,
} from '$/providers/openedProjects/projectNames'
import { clamp } from '$/utils/data/math'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { arrayEquals } from '@/util/equals'
import { parseMethodPointer, type MethodCall } from '@/util/methodPointer'
import type { ProjectPath } from '@/util/projectPath'
import { isSome } from 'enso-common/src/utilities/data/opt'
import { Ok, type Result } from 'enso-common/src/utilities/data/result'
import { markRaw } from 'vue'
import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall as LSMethodCall,
  ProfilingInfo,
} from 'ydoc-shared/languageServerTypes'
import { SuggestionDb } from '../suggestionDatabase'

/**
 * Represents type information for expressions, managing both visible and hidden intersection types.
 */
export class TypeInfo {
  private constructor(
    /** The primary type is the first visible type (e.g., `A` in `A & B`). */
    public primaryType: ProjectPath,
    /**
     * A list of 'visible' intersection types, e.g., [`A`] in `(A & B) : A`, or [`A`, `B`] in `A & B`.
     * It is never empty.
     */
    public visibleTypes: ProjectPath[],
    /** A list of 'hidden' intersection types, e.g., [`B`] in `(A & B) : A` */
    public hiddenTypes: ProjectPath[],
  ) {}

  /** @returns The ancestor types of the primary type by traversing the suggestion database. */
  ancestors(db: SuggestionDb): Iterable<ProjectPath> {
    const typename = this.primaryType
    if (typename == null) return []
    const entry = db.getEntryByProjectPath(typename)
    if (entry == null) return []
    return db.ancestors(entry)
  }

  /** Create TypeInfo from already parsed types. */
  static fromParsedTypes(
    visibleTypes: ProjectPath[],
    hiddenTypes: ProjectPath[],
  ): TypeInfo | undefined {
    const primaryType = visibleTypes[0]
    if (primaryType == null) return undefined
    return new TypeInfo(primaryType, visibleTypes, hiddenTypes)
  }

  /** Create TypeInfo from language server response. */
  static fromLsResponse(
    visibleTypes: string[],
    hiddenTypes: string[],
    projectNames: ProjectNameStore,
  ): TypeInfo | undefined {
    const processedHiddenTypes = hiddenTypes
      .map((t) => tryParseProjectPath(t, projectNames))
      .filter(isSome)
    const processedVisibleTypes = visibleTypes
      .map((t) => tryParseProjectPath(t, projectNames))
      .filter(isSome)
    const primaryType = processedVisibleTypes[0]
    if (primaryType == null) return undefined
    return new TypeInfo(primaryType, processedVisibleTypes, processedHiddenTypes)
  }

  /** Check if this TypeInfo equals another */
  equals(other: TypeInfo | undefined): boolean {
    if (other == null) return false
    return (
      arrayEquals(this.visibleTypes, other.visibleTypes, (a, b) => a.equals(b)) &&
      arrayEquals(this.hiddenTypes, other.hiddenTypes, (a, b) => a.equals(b))
    )
  }
}

function tryParseProjectPath(
  path: string,
  projectNames: ProjectNameStore,
): ProjectPath | undefined {
  const parsed = projectNames.parseProjectPathRaw(path)
  if (!parsed.ok) {
    parsed.error.log(`Could not parse '${path}' as a project path`)
    return undefined
  }
  return parsed.value
}

export interface ExpressionInfo {
  typeInfo: TypeInfo | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
  /**
   * This value is incremented when a new evaluation is reported, so that when interpreting the
   * `progress` in a `Pending` payload, we can distinguish an incremental update from a separate
   * evaluation.
   */
  evaluationId: number
}

/**
 * If the given expression is currently evaluating, returns the progress as a percentage. Otherwise,
 * returns `undefined`.
 */
export function evaluationProgress(expressionInfo: ExpressionInfo | undefined): number | undefined {
  return payloadProgress(expressionInfo?.payload)
}

function payloadProgress(payload: ExpressionUpdatePayload | undefined): number | undefined {
  if (!payload) return
  if (payload.type !== 'Pending') return
  const rawProgress = payload.progress
  if (rawProgress == null || rawProgress === -1) return
  return clamp(rawProgress * 100, 0, 100)
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
      if (info) {
        updateInfo(info, update, this.projectNames)
      } else {
        this.db.set(update.expressionId, combineInfo(undefined, update, this.projectNames))
      }
    }
  }

  /** TODO: Add docs */
  getExpressionInfo(exprId: ExpressionId | undefined): ExpressionInfo | undefined {
    return exprId == null ? undefined : this.db.get(exprId)
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
  if (newInfo.typeInfo && !newInfo.typeInfo.equals(info.typeInfo)) info.typeInfo = newInfo.typeInfo
  if (newInfo.methodCall !== info.methodCall) info.methodCall = newInfo.methodCall
  if (newInfo.payload !== info.payload) info.payload = newInfo.payload
  if (newInfo.profilingInfo !== info.profilingInfo) info.profilingInfo = newInfo.profilingInfo
  if (newInfo.evaluationId !== info.evaluationId) info.evaluationId = newInfo.evaluationId
  // Ensure new fields can't be added to `ExpressionInfo` without this code being updated.
  const _allFieldsHandled = {
    typeInfo: newInfo.typeInfo,
    methodCall: newInfo.methodCall,
    payload: newInfo.payload,
    profilingInfo: newInfo.profilingInfo,
    evaluationId: newInfo.evaluationId,
  } satisfies ExpressionInfo
}

/**
 * Translate the MethodCall retrieved from language server to our structure.
 *
 * The qualified names are validated and stored as {@link ProjectPath}s.
 */
export function translateMethodCall(
  ls: LSMethodCall,
  projectNames: ProjectNameStore,
): Result<MethodCall> {
  const methodPointer = parseMethodPointer(ls.methodPointer, projectNames)
  if (!methodPointer.ok) return methodPointer
  return Ok({
    methodPointer: methodPointer.value,
    notAppliedArguments: ls.notAppliedArguments,
  })
}

function combineInfo(
  info: ExpressionInfo | undefined,
  update: ExpressionUpdate,
  projectNames: ProjectNameStore,
): ExpressionInfo {
  const isPending = update.payload.type === 'Pending'

  // Create typeInfo from the update, or preserve existing when pending
  const typeInfo =
    isPending && info?.typeInfo ?
      info.typeInfo
    : TypeInfo.fromLsResponse(update.type, update.hiddenType ?? [], projectNames)

  const newMethodCall =
    update.methodCall ? translateMethodCall(update.methodCall, projectNames) : undefined
  if (newMethodCall && !newMethodCall.ok) {
    newMethodCall.error.log('Discarding invalid methodCall in expression update')
  }
  const evaluationId =
    info ?
      updateProgressIsNewEvaluation(info?.payload, update.payload) ? info.evaluationId + 1
      : info.evaluationId
    : 0
  return {
    typeInfo,
    methodCall:
      newMethodCall?.ok ? newMethodCall.value
      : isPending ? info?.methodCall
      : undefined,
    payload: update.payload,
    profilingInfo: update.profilingInfo,
    evaluationId,
  }
}

function updateProgressIsNewEvaluation(
  payload0: ExpressionUpdatePayload,
  payload1: ExpressionUpdatePayload,
) {
  const progress1 = payloadProgress(payload1)
  // Current evaluation completed.
  if (progress1 == null) return false
  const progress0 = payloadProgress(payload0)
  // New evaluation started.
  if (progress0 == null) return true
  // The backend guarantees that updates are monotonic, so if progress decreases, we can assume a
  // new evaluation started (although in that case we should have received a new payload without
  // progress in the interim).
  if (progress1 < progress0) return true
  // Incremental update.
  return false
}
