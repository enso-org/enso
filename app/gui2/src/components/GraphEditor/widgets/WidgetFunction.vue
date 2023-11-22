<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Tree } from '@/generated/ast'
import { defineWidget, Score, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { AstExtended } from '@/util/ast'
import { ArgumentApplication } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()

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
  <NodeWidget :input="application" />
</template>
