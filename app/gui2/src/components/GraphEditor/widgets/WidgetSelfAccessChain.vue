<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { computed } from 'vue'
import { DisplayIcon } from './WidgetSelfIcon.vue'

const props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()

const iconInput = computed(() => {
  const lhs = props.input.value.lhs
  return (
    lhs && {
      ...WidgetInput.FromAstWithPort(lhs),
      [DisplayIcon]: tree.icon,
    }
  )
})

// Do not trim calls starting with capital letter. Those are usually "static dispatches", and we
// don't want to hide them. Does not check actual method suggestion info to avoid flickering before
// expression info is loaded. We are already scoped to simple access chain in self position, so
// this check should be accurate in practice.
const showFullAccessChain = computed(() => props.input.value.lhs?.code().match(/^[A-Z]/))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.PropertyAccess),
  {
    priority: 999,
    score: (info) => {
      const tree = injectWidgetTree()
      const match =
        tree.potentialSelfArgumentId != null &&
        info.input.value.lhs?.id === tree.potentialSelfArgumentId
      return match ? Score.Good : Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelfAccessChain">
    <NodeWidget v-if="iconInput" :input="iconInput" />
    <NodeWidget v-if="showFullAccessChain" :input="props.input" />
    <NodeWidget
      v-else-if="props.input.value.rhs"
      :input="WidgetInput.FromAst(props.input.value.rhs)"
    />
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
