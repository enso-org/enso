import type { ProjectStore } from '$/providers/openedProjects/project'
import type { NodeVisualizationConfiguration } from '$/providers/openedProjects/project/executionContext'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import { Ast } from '@/util/ast'
import { ProjectPath } from '@/util/projectPath'
import type { QualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { andThen, Err, Ok, type Result, unwrapOr } from 'enso-common/src/utilities/data/result'
import { computed, toRef } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'
import * as z from 'zod'

export interface TableColumnsOptions {
  project: ProjectStore
  projectNames: ToValue<Opt<ProjectNameStore>>
  /** The table. */
  expressionId: ToValue<Opt<ExternalId>>
}
/** Returns the names of the columns in the table. */
export function useTableColumns(options: TableColumnsOptions) {
  const { project, projectNames, expressionId } = options

  const visConfig = useColumnNamesVisConfig({ project, projectNames, expressionId })

  const data = project.useVisualizationData(visConfig)
  return computed((): string[] | undefined =>
    unwrapOr(andThen(data.value, parseColumnNames), undefined),
  )
}

const COLUMNS_NAMES_SCHEMA = z.array(z.string())
const parseColumnNames = (data: unknown) =>
  resultFromZodResult(COLUMNS_NAMES_SCHEMA.safeParse(data))

function resultFromZodResult<T>(result: z.SafeParseReturnType<unknown, T>): Result<T> {
  return result.success ? Ok(result.data) : Err(result.error.issues[0]?.message)
}

interface ColumnNamesVisConfigOptions {
  project: ToValue<ProjectStore>
  projectNames: ToValue<Opt<ProjectNameStore>>
  expressionId: ToValue<Opt<ExternalId>>
}
function useColumnNamesVisConfig(options: ColumnNamesVisConfigOptions) {
  const project = toRef(options.project)
  const projectNames = toRef(options.projectNames)
  const expressionId = toRef(options.expressionId)

  const currentModule = computed(() =>
    projectNames.value?.serializeProjectPathForBackend(
      unwrapOr(project.value?.moduleProjectPath, undefined),
    ),
  )

  return computed((): NodeVisualizationConfiguration | undefined =>
    currentModule.value == null || expressionId.value == null ?
      undefined
    : {
        expressionId: expressionId.value,
        visualizationModule: currentModule.value,
        expression: COLUMNS_METHOD,
        positionalArgumentsExpressions: [],
      },
  )
}

const GET_COLUMNS_METHOD = 'column_names_json' as Ast.Identifier
const WIDGETS_ENSO_PATH = ProjectPath.create(
  'Standard.Visualization' as QualifiedName,
  'Widgets' as Ast.Identifier,
)
const COLUMNS_METHOD = {
  module: WIDGETS_ENSO_PATH,
  definedOnType: WIDGETS_ENSO_PATH,
  name: GET_COLUMNS_METHOD,
}
