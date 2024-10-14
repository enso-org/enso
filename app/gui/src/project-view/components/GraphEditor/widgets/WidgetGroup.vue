<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
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
  <div class="WidgetGroup">
    <span v-if="displayParenthesis" class="token widgetApplyPadding">(</span>
    <NodeWidget v-if="child" :input="child" />
    <span v-if="displayParenthesis" class="token widgetApplyPadding">)</span>
  </div>
</template>

<style scoped>
.WidgetGroup {
  display: flex;
  align-items: center;
}

.token {
  color: rgb(255 255 255 / 0.33);
  user-select: none;
}
</style>
