<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import { injectFunctionInfo } from '@/providers/functionInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
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
  const input = WidgetInput.WithPort(WidgetInput.FromAst(lhs))
  input[DisplayIcon] = { icon: displayedIcon.value, showContents: showFullAccessChain.value }
  return input
})

// Do not trim calls starting with capital letter. Those are usually "static dispatches", and we
// don't want to hide them. Does not check actual method suggestion info to avoid flickering before
// expression info is loaded. We are already scoped to simple access chain in self position, so
// this check should be accurate in practice.
const showFullAccessChain = computed(() => /^[A-Z]/.test(props.input.value.lhs?.code() ?? ''))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.PropertyAccess),
  {
    priority: 999,
    score: (info) => {
      const tree = injectWidgetTree()
      const selfId = tree.potentialSelfArgumentId
      const match = selfId != null && info.input.value.lhs?.id === selfId
      return match ? Score.Good : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelfAccessChain" :class="{ showFullAccessChain }">
    <NodeWidget v-if="iconInput" :input="iconInput" />
    <NodeWidget
      v-if="showFullAccessChain"
      :input="WidgetInput.FromAst(props.input.value.operator)"
    />
    <NodeWidget v-if="props.input.value.rhs" :input="WidgetInput.FromAst(props.input.value.rhs)" />
  </div>
</template>

<style scoped>
.WidgetSelfAccessChain {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: var(--widget-token-pad-unit);
  &.showFullAccessChain {
    gap: 0;
  }
}
</style>
