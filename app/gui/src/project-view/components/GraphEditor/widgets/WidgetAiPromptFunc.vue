<script setup lang="ts">
import { useGraphStore } from '$/components/WithCurrentProject.vue'
import { asNodeId } from '$/providers/openedProjects/graph/graphDatabase'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import { AI_ICON, isAiAssignment, readAiPrompt } from '@/components/GraphEditor/aiNode'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { nodeDocumentationText } from '@/util/ast/node'
import { useDisplayedIcon } from '@/util/getIconName'
import { computed, toRef } from 'vue'

defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const tree = injectWidgetTree()

const prompt = computed(() => {
  const nodeId = asNodeId(tree.externalId)
  const owner = nodeId != null ? graph.db.nodeIdToNode.get(nodeId) : undefined
  return owner ? readAiPrompt(nodeDocumentationText(owner)) : null
})

const { displayedIcon } = useDisplayedIcon(graph.db, toRef(tree, 'externalId'), AI_ICON)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [WidgetInput.astMatcher(Ast.Ident), WidgetInput.astMatcher(Ast.PropertyAccess)],
  {
    // Lower than `WidgetFunctionName` (2) so we win the function-token slot before its editable
    // name widget latches on once the engine resolves the generated UDC's methodPointer.
    priority: 1,
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
  <div class="WidgetAiPromptFunc widgetParent">
    <GrowingSpinner
      v-if="displayedIcon === '$evaluating'"
      class="aiIcon grab-handle"
      :size="16"
      phase="loading-medium"
    />
    <SvgIcon v-else class="aiIcon grab-handle" :name="displayedIcon" />
    <span class="prompt widgetApplyPadding">{{ prompt ?? '' }}</span>
  </div>
</template>

<style scoped>
.WidgetAiPromptFunc {
  display: inline-flex;
  align-items: center;
  gap: var(--widget-token-pad-unit);
}

.aiIcon {
  flex: none;
}

.prompt {
  white-space: nowrap;
}
</style>
