<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectFunctionInfo, provideFunctionInfo } from '@/providers/functionInfo'
import type { PortId } from '@/providers/portInfo'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { widgetConfigurationSchema } from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore, type NodeVisualizationConfiguration } from '@/stores/project'
import { Ast } from '@/util/ast'
import { ArgumentApplication } from '@/util/callTree'
import type { Opt } from '@/util/opt'
import type { ExprId } from 'shared/yjsModel'
import { computed, proxyRefs } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const project = useProjectStore()

provideFunctionInfo(
  proxyRefs({
    callId: computed(() => props.input.exprId),
  }),
)

const methodCallInfo = computed(() => {
  const input: Ast.Ast = props.input
  return graph.db.getMethodCallInfo(input.exprId)
})

const interpreted = computed(() => {
  return ArgumentApplication.Interpret(props.input, methodCallInfo.value == null)
})

const application = computed(() => {
  const call = interpreted.value
  if (!call) return props.input
  const noArgsCall = call.kind === 'prefix' ? graph.db.getMethodCall(call.func.exprId) : undefined

  const info = methodCallInfo.value
  return ArgumentApplication.FromInterpretedWithInfo(
    call,
    noArgsCall,
    info?.methodCall,
    info?.suggestion,
    !info?.staticallyApplied,
  )
})

const escapeString = (str: string): string => {
  const escaped = str.replaceAll(/([\\'])/g, '\\$1')
  return `'${escaped}'`
}
const makeArgsList = (args: string[]) => '[' + args.map(escapeString).join(', ') + ']'

const selfArgumentExprId = computed<Opt<ExprId>>(() => {
  const analyzed = ArgumentApplication.Interpret(props.input, true)
  if (analyzed.kind === 'infix') {
    return analyzed.lhs?.exprId
  } else {
    return analyzed.args[0]?.argument.exprId
  }
})

const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
  const tree = props.input
  const expressionId = selfArgumentExprId.value
  const astId = tree.exprId
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
  const data = visualizationData.value
  if (data != null && data.ok) {
    const parseResult = widgetConfigurationSchema.safeParse(data.value)
    if (parseResult.success) {
      return parseResult.data
    } else {
      console.error('Unable to parse widget configuration.', data, parseResult.error)
    }
  }
  return undefined
})

function handleArgUpdate(value: unknown, origin: PortId): boolean {
  const app = application.value
  // TODO: placeholder argument update
  console.log('handleArgUpdate', value, origin, app)
  if (app instanceof ArgumentApplication) {
    const args = app.allArguments()

    // const arg = app.args.find((arg) => arg.argument.exprId === origin)
    // if (arg != null) {
    //   const newArg = { ...arg, argument: value }
    //   const newCall = { ...app, args: app.args.map((arg) => (arg === newArg ? newArg : arg)) }
    //   const newExpr = ArgumentApplication.ToAst(newCall)
    //   if (newExpr != null) {
    //     graph.db.updateExpr(props.input.exprId, newExpr)
    //     return true
    //   }
    // }
  }
  return false
}
</script>
<script lang="ts">
export const widgetDefinition = defineWidget([Ast.App, Ast.Ident, Ast.OprApp], {
  priority: -10,
  score: (props, db) => {
    const ast = props.input
    if (ast.exprId == null) return Score.Mismatch
    const prevFunctionState = injectFunctionInfo(true)

    // It is possible to try to render the same function application twice, e.g. when detected an
    // application with no arguments applied yet, but the application target is also an infix call.
    // In that case, the reentrant call method info must be ignored to not create an infinite loop,
    // and to resolve the infix call as its own application.
    if (prevFunctionState?.callId === ast.exprId) return Score.Mismatch

    if (ast instanceof Ast.App || ast instanceof Ast.OprApp) return Score.Perfect

    const info = db.getMethodCallInfo(ast.exprId)
    if (prevFunctionState != null && info?.staticallyApplied === true && ast instanceof Ast.Ident) {
      return Score.Mismatch
    }
    return info != null ? Score.Perfect : Score.Mismatch
  },
})
</script>

<template>
  <NodeWidget :input="application" :dynamicConfig="widgetConfiguration" @update="handleArgUpdate" />
</template>
