<script setup lang="ts">
import { useGraphStore } from '$/components/WithCurrentProject.vue'
import { asNodeId } from '$/providers/openedProjects/graph/graphDatabase'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import { isAiAssignment, readAiPrompt } from '@/components/GraphEditor/aiNode'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { nodeDocumentationText } from '@/util/ast/node'
import { computed } from 'vue'

defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const tree = injectWidgetTree()

const prompt = computed(() => {
  const nodeId = asNodeId(tree.externalId)
  const owner = nodeId != null ? graph.db.nodeIdToNode.get(nodeId) : undefined
  return owner ? readAiPrompt(nodeDocumentationText(owner)) : null
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [WidgetInput.astMatcher(Ast.Ident), WidgetInput.astMatcher(Ast.PropertyAccess)],
  {
    // Lower than `WidgetFunctionName` (2) so we win the function-token slot before its editable
    // name widget latches on once the engine resolves the generated UDC's methodPointer.
    // But WidgetIcon should still take precedence.
    priority: 2,
    score: (info, db) => {
      const tree = injectWidgetTree()
      const nodeId = asNodeId(tree.externalId)
      const owner = nodeId != null ? db.nodeIdToNode.get(nodeId) : undefined
      if (!owner || !isAiAssignment(owner.outerAst)) return Score.Mismatch
      // `primaryApplication.function` is null whenever the call's subject is a type or
      // constructor (e.g. `Main.ai_component …`), which is exactly how the generated UDC call
      // is shaped. Compute the innermost-App function directly to cover that case.
      let fn: Ast.Expression = owner.innerExpr
      while (fn instanceof Ast.App) fn = fn.function
      return info.input.value.id === fn.id ? Score.Perfect : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <span class="WidgetAiPrompt widgetSingleLine widgetApplyPadding">{{ prompt ?? '' }}</span>
</template>

<style scoped>
.WidgetAiPrompt {
  white-space: nowrap;
}
</style>
