<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectFunctionInfo, provideFunctionInfo } from '@/providers/functionInfo'
import type { WidgetInput } from '@/providers/widgetRegistry'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import {
  argsWidgetConfigurationSchema,
  functionCallConfiguration,
  type FunctionCall,
} from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore, type NodeVisualizationConfiguration } from '@/stores/project'
import { Ast } from '@/util/ast'
import {
  ArgumentApplication,
  SoCalledExpression,
  getAccessOprSubject,
  interpretCall,
} from '@/util/callTree'
import type { Opt } from '@/util/data/opt'
import type { ExprId } from 'shared/yjsModel'
import { computed, proxyRefs } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const project = useProjectStore()

const inputAst = computed(() =>
  props.input instanceof SoCalledExpression ? props.input.ast : props.input,
)

provideFunctionInfo(
  proxyRefs({
    callId: computed(() => inputAst.value.astId),
  }),
)

const methodCallInfo = computed(() => {
  const astId = inputAst.value.astId
  if (astId == null) return null
  return graph.db.getMethodCallInfo(astId)
})

const interpreted = computed(() => {
  return interpretCall(inputAst.value, methodCallInfo.value == null)
})

const application = computed(() => {
  const analyzed = interpreted.value
  if (!analyzed) return props.input
  const noArgsCall =
    analyzed.kind === 'prefix' ? graph.db.getMethodCall(analyzed.func.astId) : undefined

  const info = methodCallInfo.value
  return ArgumentApplication.FromInterpretedWithInfo(
    analyzed,
    {
      noArgsCall,
      appMethodCall: info?.methodCall,
      suggestion: info?.suggestion,
      widgetCfg: widgetConfiguration.value ?? undefined,
    },
    !info?.staticallyApplied,
  )
})

const inheritedCfg = computed(() =>
  props.input instanceof SoCalledExpression ? props.input.widgetConfig : undefined,
)

const escapeString = (str: string): string => {
  const escaped = str.replaceAll(/([\\'])/g, '\\$1')
  return `'${escaped}'`
}
const makeArgsList = (args: string[]) => '[' + args.map(escapeString).join(', ') + ']'

const selfArgumentAstId = computed<Opt<ExprId>>(() => {
  const analyzed = interpretCall(inputAst.value, true)
  if (analyzed.kind === 'infix') {
    return analyzed.lhs?.astId
  } else if (analyzed.kind === 'prefix') {
    const knownArguments = methodCallInfo.value?.suggestion?.arguments
    const selfArgument =
      knownArguments?.[0]?.name === 'self'
        ? getAccessOprSubject(analyzed.func)
        : analyzed.args[0]?.argument
    return selfArgument?.astId
  } else {
    return null
  }
})

const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
  // If we inherit dynamic config, there is no point in attaching visualization.
  if (inheritedCfg.value) return null
  const expressionId = selfArgumentAstId.value
  const astId = inputAst.value.astId
  if (astId == null || expressionId == null) return null
  const info = graph.db.getMethodCallInfo(astId)
  if (!info) return null
  const args = info.suggestion.annotations
  if (args.length === 0) return null
  const name = info.suggestion.name
  return {
    expressionId,
    visualizationModule: 'Standard.Visualization.Widgets',
    expression: {
      module: 'Standard.Visualization.Widgets',
      definedOnType: 'Standard.Visualization.Widgets',
      name: 'get_widget_json',
    },
    positionalArgumentsExpressions: [`.${name}`, makeArgsList(args)],
  }
})

const visualizationData = project.useVisualizationData(visualizationConfig)
const widgetConfiguration = computed(() => {
  if (inheritedCfg.value) return inheritedCfg.value
  const data = visualizationData.value
  if (data != null && data.ok) {
    const parseResult = argsWidgetConfigurationSchema.safeParse(data.value)
    if (parseResult.success) {
      return functionCallConfiguration(parseResult.data)
    } else {
      console.error('Unable to parse widget configuration.', data, parseResult.error)
    }
  }
  return null
})
</script>
<script lang="ts">
function isFunctionCall(
  input: WidgetInput,
): input is
  | (SoCalledExpression & { ast: Ast.App | Ast.Ident | Ast.OprApp; widgetConfig: FunctionCall })
  | Ast.App
  | Ast.Ident
  | Ast.OprApp {
  if (input instanceof Ast.App || input instanceof Ast.Ident || input instanceof Ast.OprApp)
    return true
  if (input instanceof SoCalledExpression && input.widgetConfig?.kind === 'FunctionCall')
    return true
  return false
}

export const widgetDefinition = defineWidget(isFunctionCall, {
  priority: -10,
  score: (props, db) => {
    const ast = props.input instanceof SoCalledExpression ? props.input.ast : props.input
    if (ast.astId == null) return Score.Mismatch
    const prevFunctionState = injectFunctionInfo(true)

    // It is possible to try to render the same function application twice, e.g. when detected an
    // application with no arguments applied yet, but the application target is also an infix call.
    // In that case, the reentrant call method info must be ignored to not create an infinite loop,
    // and to resolve the infix call as its own application.
    if (prevFunctionState?.callId === ast.astId) return Score.Mismatch

    if (ast instanceof Ast.App || ast instanceof Ast.OprApp) return Score.Perfect

    const info = db.getMethodCallInfo(ast.astId)
    if (prevFunctionState != null && info?.staticallyApplied === true && ast instanceof Ast.Ident) {
      return Score.Mismatch
    }
    return info != null ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <NodeWidget :input="application" />
</template>
