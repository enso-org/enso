<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import { injectFunctionInfo } from '@/providers/functionInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { unwrapGroups } from '@/util/ast/abstract'
import { displayedIconOf, useDisplayedIcon } from '@/util/getIconName'
import { computed, toRef } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const functionInfo = injectFunctionInfo(true)
const graph = useGraphStore()
const tree = injectWidgetTree()

const baseIcon = computed(() => {
  const callInfo = functionInfo?.callInfo
  return displayedIconOf(
    callInfo?.suggestion,
    callInfo?.methodCall.methodPointer,
    functionInfo?.outputType ?? 'Unknown',
  )
})
const { displayedIcon } = useDisplayedIcon(graph.db, toRef(tree, 'externalId'), baseIcon)

const iconInput = computed(() => {
  const lhs = props.input.value.lhs
  if (!lhs) return
  const expression = unwrapGroups(lhs)
  let portId = expression.id
  if (expression instanceof Ast.TypeAnnotated) {
    // The PortId of the type annotated expression is the internal expression,
    // not the whole Ast.TypeAnnotated, so that the connections are displayed correctly.
    const innerExpr = unwrapGroups(expression.expression)
    portId = innerExpr.id
  }
  const input = WidgetInput.WithPort(WidgetInput.FromAstWithPortId(expression, portId))
  const isTypeAnnotated = expression instanceof Ast.TypeAnnotated
  input[DisplayIcon] = {
    icon: displayedIcon.value,
    showContents: isTypeAnnotated,
    noGap: isTypeAnnotated,
  }
  return input
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.PropertyAccess),
  {
    priority: 999,
    score: (info) => {
      const tree = injectWidgetTree()
      const selfId = tree.primaryApplication.function
      const subject = info.input.value
      if (subject.id === selfId) {
        return Score.Good
      }
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelfAccessChain">
    <NodeWidget v-if="iconInput" :input="iconInput" />
    <NodeWidget v-if="props.input.value.rhs" :input="WidgetInput.FromAst(props.input.value.rhs)" />
  </div>
</template>

<style scoped>
.WidgetSelfAccessChain {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: var(--widget-token-pad-unit);
}
</style>
