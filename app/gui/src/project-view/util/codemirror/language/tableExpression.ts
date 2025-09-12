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
import { computed, toRef, toValue, type Ref } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'

export interface TableExpressionExtensionOptions {
  project: ToValue<Opt<ProjectStore>>
  projectNames: ToValue<Opt<ProjectNameStore>>
  suggestionDb: ToValue<Opt<SuggestionDb>>
}

/** @returns a lazily initialized extension for the table expression language. */
export function useTableExpressionExtension(
  options: TableExpressionExtensionOptions,
): Readonly<Ref<Extension>> {
  const { projectNames } = options
  const project = toValue(options.project)
  const suggestionDb = toRef(options.suggestionDb)

  const columnMethodEntries = computed(() =>
    [...(suggestionDb.value?.selectableMethods(COLUMN_TYPE) ?? [])].filter(
      (method) => !EXCLUDED_COLUMN_METHODS.has(method.name),
    ),
  )
  const staticMethodEntries = computed(() => [
    ...(suggestionDb.value?.typeMethods(EXPRESSION_STATICS_TYPE) ?? []),
  ])
  const methodInfos = computed(() =>
    [...columnMethodEntries.value, ...staticMethodEntries.value].map(methodInfoFromEntry),
  )

  const columns =
    project ?
      useTableColumns({ project, projectNames, expressionId: useTableContext(true)?.externalId })
    : undefined
  return computed(() =>
    tableExpression({ methods: () => methodInfos.value, columns: () => columns?.value ?? [] }),
  )
}

const COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Column.Column' as QualifiedName,
)

const EXPRESSION_STATICS_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Internal.Expression_Statics.Expression_Statics' as QualifiedName,
)

function methodInfoFromEntry(entry: MethodSuggestionEntry): MethodCompletionInfo {
  return {
    name: entry.name,
    description: entry.documentationSummary,
    args: entry.arguments.length > 0,
  }
}

const EXCLUDED_COLUMN_METHODS = new Set([
  ///// Syntactic methods /////
  // These methods are used to implement special syntaxes in the expression language. Some cannot
  // syntactically be used as methods; others could legally be used, but the dedicated syntax is
  // preferred.
  'between',
  'iif',
  'is_in',
  'is_nothing',
  'like',
  'not',
  ///// Semantically excluded methods /////
  // These methods are available for use on Column values but may not make sense in an Expression.
  'info', // Technically works, probably not useful.
  'rename', // When used with `Column.set`, this is redundant and doesn't work.
  'to_table', // Does nothing, successfully but inefficiently.
  'to_vector', // Error
])
