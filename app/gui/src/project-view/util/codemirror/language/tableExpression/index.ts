import type { ProjectStore } from '$/providers/openedProjects/project'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import type { SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import type { MethodSuggestionEntry } from '$/providers/openedProjects/suggestionDatabase/entry'
import { useTableColumns } from '@/composables/tableColumns'
import { useTableContext } from '@/providers/tableContext'
import { ProjectPath } from '@/util/projectPath'
import type { QualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import type { Extension } from '@codemirror/state'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { tableExpression } from 'lezer-enso-table-expr'
import { computed, toRef, toValue } from 'vue'
import { tableExpressionAutocomplete } from './autocomplete'
import type { MethodCompletionInfo } from './completionData'

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
            ...suggestionDb.value.methods(NUMERIC_COLUMN_METHODS),
            ...suggestionDb.value.methods(TEXT_COLUMN_METHODS),
            ...suggestionDb.value.methods(DATE_COLUMN_METHODS),
            ...suggestionDb.value.methods(SPATIAL_COLUMN_METHODS),
            ...suggestionDb.value.methods(SPATIAL_INPUT_COLUMN_METHODS),
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
    const autocomplete = tableExpressionAutocomplete({
      methods: () => methodInfos.value,
      columns: () => columns?.value ?? [],
    })
    return [tableExpression(), autocomplete]
  }
}

const COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Column.Column' as QualifiedName,
)

const NUMERIC_COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Refined_Types.Numeric_Column.Numeric_Column' as QualifiedName,
)

const TEXT_COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Refined_Types.Text_Column.Text_Column' as QualifiedName,
)

const DATE_COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Refined_Types.Date_Column.Date_Column' as QualifiedName,
)

const SPATIAL_COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Refined_Types.Spatial_Column.Spatial_Column' as QualifiedName,
)

const SPATIAL_INPUT_COLUMN_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Refined_Types.Spatial_Column.Spatial_Input_Column' as QualifiedName,
)

const EXPRESSION_STATICS_TYPE = ProjectPath.create(
  'Standard.Table' as QualifiedName,
  'Internal.Expression_Statics.Expression_Statics' as QualifiedName,
)

const COLUMN_METHODS = {
  selfType: COLUMN_TYPE,
  name: (name: string) => !EXCLUDED_COLUMN_METHODS.has(name),
}
const NUMERIC_COLUMN_METHODS = {
  selfType: NUMERIC_COLUMN_TYPE,
  name: (name: string) => !EXCLUDED_COLUMN_METHODS.has(name),
}
const TEXT_COLUMN_METHODS = {
  selfType: TEXT_COLUMN_TYPE,
  name: (name: string) => !EXCLUDED_COLUMN_METHODS.has(name),
}
const DATE_COLUMN_METHODS = {
  selfType: DATE_COLUMN_TYPE,
  name: (name: string) => !EXCLUDED_COLUMN_METHODS.has(name),
}
const SPATIAL_COLUMN_METHODS = {
  selfType: SPATIAL_COLUMN_TYPE,
  name: (name: string) => !EXCLUDED_COLUMN_METHODS.has(name),
}
const SPATIAL_INPUT_COLUMN_METHODS = {
  selfType: SPATIAL_INPUT_COLUMN_TYPE,
  name: (name: string) => !EXCLUDED_COLUMN_METHODS.has(name),
}
const EXPRESSION_STATICS_METHODS = {
  memberOf: EXPRESSION_STATICS_TYPE,
}

function methodInfoFromEntry(entry: MethodSuggestionEntry): MethodCompletionInfo {
  return {
    name: entry.name,
    description: entry.documentationSummary,
    args: entry.arguments.length > 0,
    documentation: entry,
  }
}

const EXCLUDED_COLUMN_METHODS = new Set([
  'info', // Technically works, probably not useful.
  'rename', // When used with `Column.set`, this is redundant and doesn't work.
  'to_table', // Does nothing, successfully but inefficiently.
  'to_vector', // Error
])
