<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.PropertyAccess),
  {
    priority: 1000,
    score: (info) => {
      const tree = injectWidgetTree()
      const match =
        tree.potentialSelfArgumentId && info.input.value.lhs?.id === tree.potentialSelfArgumentId
      return match ? Score.Perfect : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelfAccessChain">
    <NodeWidget
      v-if="props.input.value.lhs"
      :input="WidgetInput.FromAstWithPort(props.input.value.lhs)"
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
}
</style>
