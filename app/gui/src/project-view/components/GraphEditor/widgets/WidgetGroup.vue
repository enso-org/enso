<script setup lang="ts">
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const child = computed(() => {
  const subExpression = props.input.value.expression
  if (subExpression)
    // Parenthesis should not affect widget hierarchy, so we pass entire configuration to expression widget.
    return {
      ...props.input,
      value: subExpression,
    }
  else return undefined
})

// There is no need to display parenthesis for top-level groups.
const displayParenthesis = computed(() => props.nesting >= 2)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.Group),
  {
    priority: 999,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetGroup widgetParent">
    <span v-if="displayParenthesis" class="token widgetSingleLine widgetApplyPadding">(</span>
    <NodeWidget v-if="child" :input="child" />
    <span v-if="displayParenthesis" class="token widgetSingleLine widgetApplyPadding">)</span>
  </div>
</template>

<style scoped>
.WidgetGroup {
  display: flex;
  align-items: stretch;
}

.token {
  opacity: 0.33;
  user-select: none;
}
</style>
