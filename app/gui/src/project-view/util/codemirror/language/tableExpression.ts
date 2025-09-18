import { useTableColumns } from '@/composables/tableColumns'
import { useTableContext } from '@/providers/tableContext'
import type { ProjectStore } from '@/stores/project'
import type { ProjectNameStore } from '@/stores/projectNames'
import type { SuggestionDb } from '@/stores/suggestionDatabase'
import type { MethodSuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { ProjectPath } from '@/util/projectPath'
import type { QualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import type { Extension } from '@codemirror/state'
import { tableExpression, type MethodCompletionInfo } from 'lezer-enso-table-expr'
import { computed, toRef, toValue } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'

export interface TableExpressionExtensionOptions {
  project: ToValue<Opt<ProjectStore>>
  projectNames: ToValue<Opt<ProjectNameStore>>
  suggestionDb: ToValue<Opt<SuggestionDb>>
}

/** @returns a lazily initialized extension for the table expression language. */
export function useTableExpressionExtension(
  options: TableExpressionExtensionOptions,
): () => Extension {
  const { projectNames } = options
  const suggestionDb = toRef(options.suggestionDb)
  const methodInfos = computed(() =>
    suggestionDb.value == null ?
      []
    : Array.from(
        new Map(
          [
            ...suggestionDb.value.methods(EXPRESSION_STATICS_METHODS),
            ...suggestionDb.value.methods(COLUMN_METHODS),
          ].map((entry) => [entry.name, entry]),
        ).values(),
        methodInfoFromEntry,
      ),
  )
  return () => {
    const project = toValue(options.project)
    const columns =
      project ?
        useTableColumns({ project, projectNames, expressionId: useTableContext(true)?.externalId })
      : undefined
    return tableExpression({
      methods: () => methodInfos.value,
      columns: () => columns?.value ?? [],
    })
  }
}

const COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Column.Column' as QualifiedName,
)

const EXPRESSION_STATICS_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Internal.Expression_Statics.Expression_Statics' as QualifiedName,
)

const EXPRESSION_STATICS_METHODS = {
  memberOf: EXPRESSION_STATICS_TYPE,
}
const COLUMN_METHODS = {
  selfType: COLUMN_TYPE,
  name: (name: string) => !EXCLUDED_COLUMN_METHODS.has(name),
}

function methodInfoFromEntry(entry: MethodSuggestionEntry): MethodCompletionInfo {
  return {
    name: entry.name,
    description: entry.documentationSummary,
    args: entry.arguments.length > 0,
  }
}

const EXCLUDED_COLUMN_METHODS = new Set([
  'iif', // Used to implement `if`; no reason to call it as a method.
  'info', // Technically works, probably not useful.
  'rename', // When used with `Column.set`, this is redundant and doesn't work.
  'to_table', // Does nothing, successfully but inefficiently.
  'to_vector', // Error
])
