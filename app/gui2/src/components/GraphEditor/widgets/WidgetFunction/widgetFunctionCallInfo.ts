import type { MethodCallInfo } from '@/stores/graph/graphDatabase'
import type { ExpressionInfo } from '@/stores/project/computedValueRegistry'
import type { NodeVisualizationConfiguration } from '@/stores/project/executionContext'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import {
  ArgumentApplication,
  getAccessOprSubject,
  getMethodCallInfoRecursively,
  interpretCall,
} from '@/util/callTree'
import type { ToValue } from '@/util/reactivity'
import type { Opt } from 'shared/util/data/opt'
import type { ExternalId } from 'shared/yjsModel'
import { computed, toValue } from 'vue'

export const WIDGETS_ENSO_MODULE = 'Standard.Visualization.Widgets'
export const GET_WIDGETS_METHOD = 'get_widget_json'

/**
 * A composable gathering information about call for WidgetFunction basing on AST and
 * expression updates.
 */
export function useWidgetFunctionCallInfo(
  inputAst: ToValue<Ast.Ast>,
  graphDb: {
    getMethodCallInfo(id: AstId): MethodCallInfo | undefined
    getExpressionInfo(id: AstId): ExpressionInfo | undefined
  },
) {
  const methodCallInfo = computed(() => {
    return getMethodCallInfoRecursively(toValue(inputAst), graphDb)
  })

  const interpreted = computed(() => {
    return interpretCall(toValue(inputAst), methodCallInfo.value == null)
  })

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

  const selfArgumentExternalId = computed<Opt<ExternalId>>(() => {
    const analyzed = interpreted.value
    if (analyzed.kind === 'infix') {
      return analyzed.lhs?.externalId
    } else if (methodCallInfo.value?.suggestion.selfType != null) {
      const knownArguments = methodCallInfo.value?.suggestion?.arguments
      const hasSelfArgument = knownArguments?.[0]?.name === 'self'
      const selfArgument =
        hasSelfArgument && !selfArgumentPreapplied.value ?
          analyzed.args.find((a) => a.argName === 'self' || a.argName == null)?.argument
        : getAccessOprSubject(analyzed.func) ?? analyzed.args[0]?.argument

      return selfArgument?.externalId
    } else {
      return null
    }
  })

  const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
    const args = ArgumentApplication.collectArgumentNamesAndUuids(
      interpreted.value,
      methodCallInfo.value,
    )

    const selfArgId = selfArgumentExternalId.value
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
    if (selfArgId != null) {
      return {
        expressionId: selfArgId,
        visualizationModule: WIDGETS_ENSO_MODULE,
        expression: {
          module: WIDGETS_ENSO_MODULE,
          definedOnType: WIDGETS_ENSO_MODULE,
          name: GET_WIDGETS_METHOD,
        },
        positionalArgumentsExpressions,
      }
    } else {
      // In the case when no self argument is present (for example in autoscoped constructor),
      // we assume that this is a static function call.
      return {
        expressionId: toValue(inputAst).externalId,
        visualizationModule: WIDGETS_ENSO_MODULE,
        expression: `_ -> ${WIDGETS_ENSO_MODULE}.${GET_WIDGETS_METHOD} ${info.suggestion.definedIn}`,
        positionalArgumentsExpressions,
      }
    }
  })

  return {
    interpreted,
    methodCallInfo,
    selfArgumentPreapplied,
    subjectInfo,
    visualizationConfig,
  }
}
