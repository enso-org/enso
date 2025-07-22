import { type GraphStore } from '@/stores/graph'
import { type ProjectStore } from '@/stores/project'
import { type ComputedValueRegistry } from '@/stores/project/computedValueRegistry'
import { valueExt, type ValueExt } from '@/util/codemirror/stateEffect'
import { type ToValue } from '@/util/reactivity'
import { type Diagnostic, forceLinting, linter } from '@codemirror/lint'
import { type Extension } from '@codemirror/state'
import { type EditorView } from '@codemirror/view'
import * as iter from 'enso-common/src/utilities/data/iter'
import { computed, type ComputedRef, toValue, watchEffect } from 'vue'
import { type Diagnostic as LSDiagnostic } from 'ydoc-shared/languageServerTypes'
import { type SourceRange } from 'ydoc-shared/util/data/text'
import { type ExternalId } from 'ydoc-shared/yjsModel'

/**
 * CodeMirror extension providing diagnostics for an Enso module. Provides CodeMirror diagnostics
 * based on dataflow errors and diagnostics the LS provided in an `executionStatus` message.
 */
export function useEnsoDiagnostics(
  projectStore: Pick<ProjectStore, 'computedValueRegistry' | 'dataflowErrors' | 'diagnostics'>,
  graphStore: Pick<GraphStore, 'moduleSource' | 'db'>,
  view: EditorView,
): Extension {
  function spanOfExternalId(externalId: ExternalId): SourceRange | undefined {
    const astId = graphStore.db.idFromExternal(externalId)
    return astId && graphStore.moduleSource.getSpan(astId)
  }
  return [
    reactiveDiagnostics(
      view,
      expressionUpdateDiagnostics,
      useExpressionUpdateDiagnostics({
        spanOfExternalId,
        computedValueDb: projectStore.computedValueRegistry.db,
        getDataflowError: (externalId: ExternalId) =>
          projectStore.dataflowErrors.lookup(externalId)?.value?.message,
      }),
    ),
    reactiveDiagnostics(
      view,
      executionContextDiagnostics,
      useExecutionContextDiagnostics({
        spanOfExternalId,
        lsDiagnostics: () => projectStore.diagnostics,
      }),
    ),
  ]
}

function reactiveDiagnostics(
  view: EditorView,
  state: ValueExt<Diagnostic[]>,
  diagnostics: ToValue<Diagnostic[]>,
) {
  watchEffect(() => {
    view.dispatch({ effects: state.set.of(toValue(diagnostics)) })
    forceLinting(view)
  })
  return [
    state.extension,
    linter((view) => view.state.field(state.get) ?? [], {
      needsRefresh: state.changed,
    }),
  ]
}

// === Expression Update Diagnostics ===

const expressionUpdateDiagnostics = valueExt<Diagnostic[]>([])

function useExpressionUpdateDiagnostics({
  spanOfExternalId,
  computedValueDb,
  getDataflowError,
}: {
  spanOfExternalId: (externalId: ExternalId) => SourceRange | undefined
  computedValueDb: ComputedValueRegistry['db']
  getDataflowError: (externalId: ExternalId) => string | undefined
}): ComputedRef<Diagnostic[]> {
  return computed<Diagnostic[]>(() => {
    const panics = computedValueDb.type.reverseLookup('Panic')
    const errors = computedValueDb.type.reverseLookup('DataflowError')
    const diagnostics: Diagnostic[] = []
    for (const externalId of iter.chain(panics, errors)) {
      const update = computedValueDb.get(externalId)
      if (!update) continue
      const span = spanOfExternalId(externalId)
      if (!span) continue
      const { from, to } = span
      switch (update.payload.type) {
        case 'Panic': {
          diagnostics.push({ from, to, message: update.payload.message, severity: 'error' })
          break
        }
        case 'DataflowError': {
          const error = getDataflowError(externalId)
          if (error) diagnostics.push({ from, to, message: error, severity: 'error' })
          break
        }
      }
    }
    return diagnostics
  })
}

// === Execution Status Diagnostics ===

const executionContextDiagnostics = valueExt<Diagnostic[]>([])

function executionContextDiagnostic(
  { from, to }: SourceRange,
  diagnostic: LSDiagnostic,
): Diagnostic {
  return {
    from,
    to,
    message: diagnostic.message,
    severity:
      diagnostic.kind === 'Error' ? 'error'
      : diagnostic.kind === 'Warning' ? 'warning'
      : 'info',
  }
}

function useExecutionContextDiagnostics({
  spanOfExternalId,
  lsDiagnostics,
}: {
  spanOfExternalId: (externalId: ExternalId) => SourceRange | undefined
  lsDiagnostics: ToValue<LSDiagnostic[]>
}): ComputedRef<Diagnostic[]> {
  return computed<Diagnostic[]>(() =>
    toValue(lsDiagnostics).flatMap((diagnostic) => {
      const span = diagnostic.expressionId && spanOfExternalId(diagnostic.expressionId)
      if (!span) return []
      return [executionContextDiagnostic(span, diagnostic)]
    }),
  )
}
