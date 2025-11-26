import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import { type ProjectStore } from '$/providers/openedProjects/project'
import { type NodeVisualizationConfiguration } from '$/providers/openedProjects/project/executionContext'
import { type ProjectNameStore } from '$/providers/openedProjects/projectNames'
import type { GroupInfo } from '$/providers/openedProjects/suggestionDatabase'
import { entryIsAnnotatable } from '$/providers/openedProjects/suggestionDatabase/entry'
import type { WidgetInput } from '$/providers/openedProjects/widgetRegistry'
import {
  argsWidgetConfigurationSchema,
  functionCallConfiguration,
  pending,
  type FunctionCall,
} from '$/providers/openedProjects/widgetRegistry/configuration'
import { Ast } from '@/util/ast'
import {
  ArgumentApplication,
  deriveLocalCallInfoFromCode,
  getAccessOprSubject,
  getMethodCallInfoRecursively,
  interpretCall,
} from '@/util/callTree'
import { type MethodPointer } from '@/util/methodPointer'
import { ProjectPath } from '@/util/projectPath'
import type { ToValue } from '@/util/reactivity'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { computed, toValue, type DeepReadonly } from 'vue'
import type { IdentifierOrOperatorIdentifier } from 'ydoc-shared/ast'
import type { ExternalId } from 'ydoc-shared/yjsModel'
import { GET_WIDGETS_METHOD, WIDGETS_ENSO_MODULE, WIDGETS_ENSO_PATH } from './consts'

/**
 * A composable gathering information about call for WidgetFunction basing on AST and
 * expression updates.
 */
export function useWidgetFunctionCallInfo(
  input: ToValue<WidgetInput & { value: Ast.Expression }>,
  graphDb: ToValue<GraphDb>,
  // Cannot be ToValue - see TODO next to visualizationData
  project: ToValue<Pick<ProjectStore, 'useVisualizationData' | 'moduleProjectPath'>>,
  projectNames: ToValue<ProjectNameStore>,
  groups: ToValue<DeepReadonly<GroupInfo[]>>,
) {
  const methodCallInfo = computed(() => {
    const _project = toValue(project)
    if (!_project.moduleProjectPath?.ok) return
    const expression = toValue(input).value
    const projectPath = _project.moduleProjectPath.value
    const ptr = getPotentialModuleFunctionPointer(expression, projectPath, projectNames, graphDb)
    const derivedLocal = ptr && deriveLocalCallInfoFromCode(ptr, expression, toValue(groups))
    if (derivedLocal) return derivedLocal
    return getMethodCallInfoRecursively(toValue(input).value, toValue(graphDb))
  })

  const interpreted = computed(() => interpretCall(toValue(input).value))

  const appFunc = computed(() =>
    interpreted.value.kind === 'prefix' ? interpreted.value.func : undefined,
  )

  const appFuncIsNodeUsage = computed(() => toValue(graphDb).isNodeUsage(appFunc.value?.id))

  const subject = computed(() => getAccessOprSubject(appFunc.value))
  const subjectInfo = computed(() => toValue(graphDb).getExpressionInfo(subject.value?.id))

  const selfArgumentPreapplied = computed(() => {
    const info = methodCallInfo.value
    const funcType = info?.methodCall.methodPointer.definedOnType
    return (
      funcType != null &&
      !subjectInfo.value?.typeInfo?.primaryType?.equals(funcType.append('type' as Ast.Identifier))
    )
  })

  const widgetQuerySubjectExpressionId = computed<Opt<ExternalId>>(() => {
    const analyzed = interpreted.value
    if (analyzed.kind === 'infix') {
      return analyzed.lhs?.externalId
    }
    const knownArguments = methodCallInfo.value?.suggestion.arguments
    const hasKnownSelfArgument = knownArguments?.[0]?.name === 'self'

    // First we always want to attach the visualization to the `self` argument,
    // whenever we can find an unambiguous expression for it.
    if (hasKnownSelfArgument && !selfArgumentPreapplied.value) {
      return analyzed.args.find((a) => a.argName === 'self' || a.argName == null)?.argument
        ?.externalId
    }

    // When no `self` argument can be resolved or it is already applied, attach to the access
    // chain subject. This will correctly handle constructors and most common cases with not
    // yet resolved methods.
    const accessSubject = getAccessOprSubject(analyzed.func)
    if (accessSubject) {
      return accessSubject.externalId
    }
    // In other cases (e.g. autoscoped expression) there is no good existing
    // expression to attach the visualization to. Fallback to synthetic type-based expression.
    return null
  })

  const annotatedArguments = computed(() => {
    const info = methodCallInfo.value
    if (!info) return null
    if (!entryIsAnnotatable(info.suggestion)) return null
    return info.suggestion.annotations
  })

  const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
    const args = ArgumentApplication.collectArgumentNamesAndUuids(
      interpreted.value,
      methodCallInfo.value,
    )

    const info = methodCallInfo.value
    if (!info) return null
    if (!entryIsAnnotatable(info.suggestion)) return null
    const annotatedArgs = annotatedArguments.value
    if (!annotatedArgs?.length) return null
    const name = info.suggestion.name
    const positionalArgumentsExpressions = [
      `.${name}`,
      Ast.Vector.build(annotatedArgs, Ast.TextLiteral.new).code(),
      Ast.TextLiteral.new(JSON.stringify(args)).code(),
    ]

    let modulePath: ProjectPath = WIDGETS_ENSO_PATH
    const projectNamesValue = toValue(projectNames)
    const _project = toValue(project)
    if (_project.moduleProjectPath?.ok) {
      modulePath = _project.moduleProjectPath.value
    }
    const moduleFqn = projectNamesValue.serializeProjectPathForBackend(modulePath)

    const expressionId = widgetQuerySubjectExpressionId.value
    if (expressionId != null) {
      return {
        expressionId,
        visualizationModule: moduleFqn,
        expression: {
          module: WIDGETS_ENSO_PATH,
          definedOnType: WIDGETS_ENSO_PATH,
          name: GET_WIDGETS_METHOD,
        },
        positionalArgumentsExpressions,
      }
    } else {
      // In the case when no clear subject expression exists (for example in autoscoped constructor),
      // we assume that this is a static function call and create the subject by using resolved type name.
      return {
        expressionId: toValue(input).value.externalId,
        visualizationModule: moduleFqn,
        expression: `_ -> ${WIDGETS_ENSO_MODULE}.${GET_WIDGETS_METHOD} ${projectNamesValue.printProjectPath(info.suggestion.memberOf)}`,
        positionalArgumentsExpressions,
      }
    }
  })

  const subjectTypeMatchesMethod = computed(() => {
    const funcType = methodCallInfo.value?.methodCall.methodPointer.definedOnType
    return (
      funcType != null &&
      subjectInfo.value?.typeInfo?.primaryType?.equals(funcType.append('type' as Ast.Identifier))
    )
  })

  const inheritedConfig = computed(() => {
    const cfg = toValue(input).dynamicConfig
    if (!cfg) return undefined
    if (cfg.kind === 'FunctionCall') return cfg
    if (cfg.kind === 'OneOfFunctionCalls' && methodCallInfo.value != null) {
      const info = methodCallInfo.value
      const fullName = info?.suggestion.definitionPath
      const autoscopedName = '..' + info?.suggestion.name
      return (
        cfg.possibleFunctions.get(toValue(projectNames).serializeProjectPathForBackend(fullName)) ??
        cfg.possibleFunctions.get(autoscopedName)
      )
    }
    return undefined
  })

  // TODO[ao]: This does not work with project change. Either useVisualizationData API must
  //  change, or useCurrentRef should not return ref.
  const visualizationData = toValue(project).useVisualizationData(visualizationConfig)

  const widgetConfiguration = computed(() => {
    const data = visualizationData.value
    if (data?.ok) {
      const parseResult = argsWidgetConfigurationSchema.safeParse(data.value)
      if (parseResult.success) {
        return functionCallConfiguration(parseResult.data, inheritedConfig.value)
      } else {
        console.error('Unable to parse widget configuration.', data, parseResult.error)
      }
    } else if (data != null && !data.ok) {
      data.error.log('Cannot load dynamic configuration')
    }
    const parameters: FunctionCall['parameters'] = new Map(inheritedConfig.value?.parameters ?? [])
    annotatedArguments.value?.forEach((name) => {
      if (parameters.get(name) == null) parameters.set(name, pending())
    })
    return {
      kind: 'FunctionCall',
      parameters,
    } satisfies FunctionCall
  })

  const application = computed(() => {
    const call = interpreted.value
    if (!call) return null
    const noArgsCall =
      call.kind === 'prefix' ? toValue(graphDb).getMethodCallInfo(call.func.id) : undefined

    return ArgumentApplication.FromInterpretedWithInfo(call, {
      suggestion: methodCallInfo.value?.suggestion,
      widgetCfg: widgetConfiguration.value,
      subjectAsSelf: selfArgumentPreapplied.value,
      notAppliedArguments:
        (
          noArgsCall != null &&
          (!subjectTypeMatchesMethod.value || noArgsCall.methodCall.notAppliedArguments.length > 0)
        ) ?
          noArgsCall.methodCall.notAppliedArguments
        : undefined,
      /**
       * If a node doesn't fully apply a function, and that node is referenced elsewhere, multiple
       * {@link MethodCallInfo}s will result. We would not want to display the same placeholders in
       * multiple places. We give priority to the source node, assuming the function is intended to
       * be fully-applied, and suppress placeholders at the use site. However, we still construct an
       * {@link ArgumentApplication}, so that if arguments *are* provided at the use site they will
       * be displayed with full information. Note that reverse-inhibition is not implemented: In
       * this case, the late-applied arguments will still have placeholders at the definition site,
       * and fulfilling those placeholders would break the calls at use sites.
       */
      suppressPlaceholders: appFuncIsNodeUsage.value,
    })
  })

  return {
    methodCallInfo,
    application,
    subject,
    subjectInfo,
  }
}

/**
 * Check if given AST potentially represents an expression that would always evaluate to local module.
 *
 * e.g. `Main`, `local.ThisProjectName.Main`
 */
export function isModuleExpression(
  expr: Ast.Expression,
  projectPath: ProjectPath,
  projectNames: ToValue<ProjectNameStore>,
) {
  if (expr instanceof Ast.Ident) return expr.token.code() === projectPath.path
  if (
    expr instanceof Ast.PropertyAccess &&
    expr.lhs instanceof Ast.PropertyAccess &&
    expr.lhs.lhs instanceof Ast.Ident
  ) {
    return expr.code() === toValue(projectNames).serializeProjectPathForBackend(projectPath)
  }
  return false
}

/**
 * Check if given AST potentially represents a locally defined method call.
 * Returns a method pointer that might potentially not represent a real method.
 */
export function getPotentialModuleFunctionPointer(
  expr: Ast.Expression,
  modulePath: ProjectPath,
  projectNames: ToValue<ProjectNameStore>,
  graphDb: ToValue<GraphDb>,
): MethodPointer | undefined {
  let candidateFunctionName: IdentifierOrOperatorIdentifier | null = null
  while (expr instanceof Ast.App) expr = expr.function
  if (expr instanceof Ast.Ident) {
    const db = toValue(graphDb)
    const definition = db.getIdentDefiningNode(expr.id)
    // Reject idents that have local definitions, as they might shadow the module method.
    if (!definition) candidateFunctionName = expr.token.code()
  } else if (
    expr instanceof Ast.PropertyAccess &&
    expr.lhs &&
    isModuleExpression(expr.lhs, modulePath, projectNames)
  ) {
    candidateFunctionName = expr.rhs.code()
  }

  if (candidateFunctionName) {
    return {
      module: modulePath,
      definedOnType: modulePath,
      name: candidateFunctionName,
    }
  }
}
