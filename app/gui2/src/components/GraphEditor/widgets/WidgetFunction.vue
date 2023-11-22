<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Tree } from '@/generated/ast'
import { defineWidget, Score, widgetProps } from '@/providers/widgetRegistry'
import { widgetConfigurationSchema } from '@/providers/widgetRegistry/configuration'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore, type NodeVisualizationConfiguration } from '@/stores/project'
import { AstExtended } from '@/util/ast'
import { ArgumentApplication } from '@/util/callTree'
import type { Opt } from '@/util/opt'
import type { ExprId } from 'shared/yjsModel'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()
const project = useProjectStore()

const application = computed(() => {
  const astId = props.input.astId
  if (astId == null) return props.input
  const info = graph.db.getMethodCallInfo(astId)
  return ArgumentApplication.FromAstWithInfo(
    props.input,
    info?.suggestion.arguments,
    info?.methodCall.notAppliedArguments ?? [],
  )
})

const selfArgumentAstId = computed<Opt<ExprId>>(() => {
  const tree = props.input
  if (tree.isTree(Tree.Type.OprApp)) {
    return tree.tryMap((tree) => tree.lhs)?.astId
  } else if (tree.isTree(Tree.Type.App)) {
    return tree.tryMap((tree) => tree.arg)?.astId
  } else if (tree.isTree(Tree.Type.NamedApp)) {
    return tree.tryMap((tree) => tree.arg)?.astId
  } else if (tree.isTree(Tree.Type.Ident)) {
    return tree.astId
  }
  return null
})

const escapeString = (str: string): string => {
  let res = "'"
  for (const c of str) {
    res += c == "'" ? "\\'" : c
  }
  return res + "'"
}
const makeArgsList = (args: string[]) => '[' + args.map(escapeString).join(', ') + ']'

const config = computed<Opt<NodeVisualizationConfiguration>>(() => {
  const tree = props.input
  const expressionId = selfArgumentAstId.value
  const astId = tree.astId
  if (astId == null || expressionId == null) {
    return null
  }
  const info = graph.db.getMethodCallInfo(astId)
  const args = info?.suggestion.arguments.map((arg) => arg.name)
  const name = info?.suggestion.name
  if (name && args) {
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
  } else {
    return null
  }
})

const visualizationData = project.useVisualizationData(config)
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
</script>
<script lang="ts">
export const widgetDefinition = defineWidget(
  AstExtended.isTree([Tree.Type.App, Tree.Type.NamedApp, Tree.Type.Ident, Tree.Type.OprApp]),
  {
    priority: 8,
    score: (props, db) => {
      const ast = props.input
      if (ast.isTree([Tree.Type.App, Tree.Type.NamedApp, Tree.Type.OprApp])) return Score.Perfect
      return ast.astId && db.isMethodCall(ast.astId) ? Score.Perfect : Score.Mismatch
    },
  },
)
</script>

<template>
  <NodeWidget :input="application" :dynamicConfig="widgetConfiguration" />
</template>
