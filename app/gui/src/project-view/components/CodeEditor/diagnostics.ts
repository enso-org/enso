import { type GraphStore } from '@/stores/graph'
import { type ProjectStore } from '@/stores/project'
import { valueExt } from '@/util/codemirror/stateEffect'
import { type Diagnostic, forceLinting, linter } from '@codemirror/lint'
import { type Extension } from '@codemirror/state'
import { type EditorView } from '@codemirror/view'
import * as iter from 'enso-common/src/utilities/data/iter'
import { computed, watchEffect } from 'vue'
import { type SourceRange } from 'ydoc-shared/util/data/text'
import { type ExternalId } from 'ydoc-shared/yjsModel'

const {
  set: setDiagnostics,
  get: getDiagnostics,
  changed: diagnosticsChanged,
  extension: stateExt,
} = valueExt<Diagnostic[], Diagnostic[] | undefined>(undefined)

/**
 * CodeMirror extension providing diagnostics for an Enso module. Provides CodeMirror diagnostics based on dataflow
 * errors, and diagnostics the LS provided in an `executionStatus` message.
 */
export function useEnsoDiagnostics(
  projectStore: Pick<ProjectStore, 'computedValueRegistry' | 'dataflowErrors' | 'diagnostics'>,
  graphStore: Pick<GraphStore, 'moduleSource' | 'db'>,
  editorView: EditorView,
): Extension {
  function spanOfExternalId(externalId: ExternalId): SourceRange | undefined {
    const astId = graphStore.db.idFromExternal(externalId)
    return astId && graphStore.moduleSource.getSpan(astId)
  }
  const expressionUpdatesDiagnostics = computed(() => {
    const updates = projectStore.computedValueRegistry.db
    const panics = updates.type.reverseLookup('Panic')
    const errors = updates.type.reverseLookup('DataflowError')
    const diagnostics: Diagnostic[] = []
    for (const externalId of iter.chain(panics, errors)) {
      const update = updates.get(externalId)
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
          const error = projectStore.dataflowErrors.lookup(externalId)
          if (error?.value?.message) {
            diagnostics.push({ from, to, message: error.value.message, severity: 'error' })
          }
          break
        }
      }
    }
    return diagnostics
  })
  const executionContextDiagnostics = computed<Diagnostic[]>(() =>
    projectStore.diagnostics.flatMap((diagnostic) => {
      const span = diagnostic.expressionId && spanOfExternalId(diagnostic.expressionId)
      if (!span) return []
      const { from, to } = span
      const severity =
        diagnostic.kind === 'Error' ? 'error'
        : diagnostic.kind === 'Warning' ? 'warning'
        : 'info'
      return [{ from, to, message: diagnostic.message, severity }]
    }),
  )
  watchEffect(() => {
    const diagnostics = [
      ...expressionUpdatesDiagnostics.value,
      ...executionContextDiagnostics.value,
    ]
    editorView.dispatch({ effects: setDiagnostics.of(diagnostics) })
    forceLinting(editorView)
  })
  return [
    stateExt,
    linter((view) => view.state.facet(getDiagnostics) ?? [], {
      needsRefresh: diagnosticsChanged,
    }),
  ]
}
