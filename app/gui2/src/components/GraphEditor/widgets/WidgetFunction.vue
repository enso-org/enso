<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
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
  (input) =>
    input instanceof Ast.App || input instanceof Ast.Ident || input instanceof Ast.OprApp,
  {
    priority: 8,
    score: (props, db) => {
      const ast: Ast.Ast = props.input
      if (ast instanceof Ast.App || ast instanceof Ast.OprApp) return Score.Perfect
      return db.isMethodCall(ast.exprId) ? Score.Perfect : Score.Mismatch
    },
  },
)
</script>

<template>
  <NodeWidget :input="application" />
</template>
