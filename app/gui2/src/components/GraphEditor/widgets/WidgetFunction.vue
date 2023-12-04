<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Tree } from '@/generated/ast'
import { widgetConfigurationSchema } from '@/providers/widgetRegistry/configuration'
import { injectFunctionInfo, provideFunctionInfo } from '@/providers/functionInfo'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore, type NodeVisualizationConfiguration } from '@/stores/project'
import { AstExtended } from '@/util/ast'
import { ArgumentApplication } from '@/util/callTree'
import type { Opt } from '@/util/opt'
import type { ExprId } from 'shared/yjsModel'
import { computed, proxyRefs, watch } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const project = useProjectStore()

provideFunctionInfo(
  proxyRefs({
    callId: computed(() => props.input.astId),
  }),
)

const methodCallInfo = computed(() => {
  const astId = props.input.astId
  if (astId == null) return null
  return graph.db.getMethodCallInfo(astId)
})

const interpreted = computed(() => {
  return ArgumentApplication.Interpret(props.input, methodCallInfo.value == null)
})

const application = computed(() => {
  const analyzed = interpreted.value
  if (!analyzed) return props.input
  const noArgsCall =
    analyzed.kind === 'prefix' ? graph.db.getMethodCall(analyzed.func.astId) : undefined

  const info = methodCallInfo.value
  return ArgumentApplication.FromInterpretedWithInfo(
    analyzed,
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

const selfArgumentAstId = computed<Opt<ExprId>>(() => {
  const analyzed = ArgumentApplication.Interpret(props.input, true)
  if (analyzed.kind === 'infix') {
    return analyzed.lhs?.astId
  } else {
    // We’re not interested in prefix applications, as functions can’t have widget annotations
    // and it’s not possible to call a method in a prefix form (any method call is an infix application).
    return null
  }
})

const visualizationConfig = computed<Opt<NodeVisualizationConfiguration>>(() => {
  const tree = props.input
  const expressionId = selfArgumentAstId.value
  const astId = tree.astId
  if (astId == null || expressionId == null) {
    return null
  }
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
  if (data != null) {
    const parseResult = widgetConfigurationSchema.safeParse(data)
    if (parseResult.success) {
      return parseResult.data
    } else {
      console.error('Unable to parse widget configuration.', data, parseResult.error)
    }
  }
  return null
})

watch(widgetConfiguration, (c) => console.log(props.input.repr(), c), { immediate: true })
</script>
<script lang="ts">
export const widgetDefinition = defineWidget(
  AstExtended.isTree([Tree.Type.App, Tree.Type.NamedApp, Tree.Type.Ident, Tree.Type.OprApp]),
  {
    priority: -10,
    score: (props, db) => {
      const ast = props.input
      if (ast.astId == null) return Score.Mismatch
      const prevFunctionState = injectFunctionInfo(true)

      // It is possible to try to render the same function application twice, e.g. when detected an
      // application with no arguments applied yet, but the application target is also an infix call.
      // In that case, the reentrant call method info must be ignored to not create an infinite loop,
      // and to resolve the infix call as its own application.
      if (prevFunctionState?.callId === ast.astId) return Score.Mismatch

      if (ast.isTree([Tree.Type.App, Tree.Type.NamedApp, Tree.Type.OprApp])) return Score.Perfect

      const info = db.getMethodCallInfo(ast.astId)
      if (
        prevFunctionState != null &&
        info?.staticallyApplied === true &&
        props.input.isTree(Tree.Type.Ident)
      ) {
        return Score.Mismatch
      }
      return info != null ? Score.Perfect : Score.Mismatch
    },
  },
)
</script>

<template>
  <NodeWidget :input="application" :dynamicConfig="widgetConfiguration" />
</template>
