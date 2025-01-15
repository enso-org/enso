import type { WidgetInput } from '@/providers/widgetRegistry'
import {
  argsWidgetConfigurationSchema,
  functionCallConfiguration,
} from '@/providers/widgetRegistry/configuration'
import type { MethodCallInfo } from '@/stores/graph/graphDatabase'
import type { ExpressionInfo } from '@/stores/project/computedValueRegistry'
import type { NodeVisualizationConfiguration } from '@/stores/project/executionContext'
import { entryQn } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import {
  ArgumentApplication,
  getAccessOprSubject,
  getMethodCallInfoRecursively,
  interpretCall,
} from '@/util/callTree'
import type { Result } from '@/util/data/result'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue, type Ref } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'
import type { ExternalId } from 'ydoc-shared/yjsModel'

export const WIDGETS_ENSO_MODULE = 'Standard.Visualization.Widgets'
export const GET_WIDGETS_METHOD = 'get_widget_json'

/**
 * A composable gathering information about call for WidgetFunction basing on AST and
 * expression updates.
 */
export function useWidgetFunctionCallInfo(
  input: ToValue<WidgetInput & { value: Ast.Expression }>,
  graphDb: {
    getMethodCallInfo(id: AstId): MethodCallInfo | undefined
    getExpressionInfo(id: AstId): ExpressionInfo | undefined
  },
  project: {
    useVisualizationData(config: Ref<Opt<NodeVisualizationConfiguration>>): Ref<Result<any> | null>
  },
) {
  const methodCallInfo = computed(() => getMethodCallInfoRecursively(toValue(input).value, graphDb))
  const interpreted = computed(() => interpretCall(toValue(input).value))

  const subjectInfo = computed(() => {
    const analyzed = interpreted.value
    if (analyzed.kind !== 'prefix') return
    const subject = getAccessOprSubject(analyzed.func)
    if (!subject) return
    return graphDb.getExpressionInfo(subject.id)
  })

  const selfArgumentPreapplied = computed(() => {
    const info = methodCallInfo.value
    const funcType = info?.methodCall.methodPointer.definedOnType
    return funcType != null && subjectInfo.value?.typename !== `${funcType}.type`
  })

  const widgetQuerySubjectExpressionId = computed<Opt<ExternalId>>(() => {
    const analyzed = interpreted.value
    if (analyzed.kind === 'infix') {
      return analyzed.lhs?.externalId
    }
    const knownArguments = methodCallInfo.value?.suggestion?.arguments
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

  const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
    const args = ArgumentApplication.collectArgumentNamesAndUuids(
      interpreted.value,
      methodCallInfo.value,
    )

    const info = methodCallInfo.value
    if (!info) return null
    const annotatedArgs = info.suggestion.annotations
    if (annotatedArgs.length === 0) return null
    const name = info.suggestion.name
    const positionalArgumentsExpressions = [
      `.${name}`,
      Ast.Vector.build(annotatedArgs, Ast.TextLiteral.new).code(),
      Ast.TextLiteral.new(JSON.stringify(args)).code(),
    ]

    const expressionId = widgetQuerySubjectExpressionId.value
    if (expressionId != null) {
      return {
        expressionId,
        visualizationModule: WIDGETS_ENSO_MODULE,
        expression: {
          module: WIDGETS_ENSO_MODULE,
          definedOnType: WIDGETS_ENSO_MODULE,
          name: GET_WIDGETS_METHOD,
        },
        positionalArgumentsExpressions,
      }
    } else {
      // In the case when no clear subject expression exists (for example in autoscoped constructor),
      // we assume that this is a static function call and create the subject by using resolved type name.
      return {
        expressionId: toValue(input).value.externalId,
        visualizationModule: WIDGETS_ENSO_MODULE,
        expression: `_ -> ${WIDGETS_ENSO_MODULE}.${GET_WIDGETS_METHOD} ${info.suggestion.memberOf ?? info.suggestion.definedIn}`,
        positionalArgumentsExpressions,
      }
    }
  })

  const subjectTypeMatchesMethod = computed(() => {
    const funcType = methodCallInfo.value?.methodCall.methodPointer.definedOnType
    return funcType != null && subjectInfo.value?.typename === `${funcType}.type`
  })

  const inheritedConfig = computed(() => {
    const cfg = toValue(input).dynamicConfig
    if (!cfg) return undefined
    if (cfg.kind === 'FunctionCall') return cfg
    if (cfg.kind === 'OneOfFunctionCalls' && methodCallInfo.value != null) {
      const info = methodCallInfo.value
      const fullName = entryQn(info?.suggestion)
      const autoscopedName = '..' + info?.suggestion.name
      return cfg.possibleFunctions.get(fullName) ?? cfg.possibleFunctions.get(autoscopedName)
    }
    return undefined
  })

  const visualizationData = project.useVisualizationData(visualizationConfig)

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
    return inheritedConfig.value
  })

  const application = computed(() => {
    const call = interpreted.value
    if (!call) return null
    const noArgsCall = call.kind === 'prefix' ? graphDb.getMethodCallInfo(call.func.id) : undefined

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
    })
  })

  return {
    methodCallInfo,
    application,
  }
}
