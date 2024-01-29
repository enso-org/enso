<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
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
</script>

<script lang="ts">
import { Ast } from '@/util/ast'

export const widgetDefinition = defineWidget(WidgetInput.astMatcher(Ast.Group), {
  priority: 999,
  score: Score.Perfect,
})
</script>

<template>
  <NodeWidget v-if="child" :input="child" />
</template>
