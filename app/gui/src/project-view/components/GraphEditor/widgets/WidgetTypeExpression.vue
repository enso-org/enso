<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Ast } from '@/util/ast'
import { computed } from 'vue'
import { EnsoExpression } from './WidgetEnsoExpression.vue'
import { withDropdownItems } from './WidgetSelection.vue'

const props = defineProps(widgetProps(widgetDefinition))
const suggestionDb = useSuggestionDbStore()

const input = computed(() => ({
  ...withDropdownItems(props.input, suggestionDb.entries.dropdownTypeExpressionTags.value),
  value: props.input.value ?? 'Any',
  [EnsoExpression]: {},
}))
</script>

<script lang="ts">
export const EnsoTypeExpression: unique symbol = Symbol.for('WidgetInput:EnsoTypeExpression')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [EnsoTypeExpression]?: object
  }
}

export const widgetDefinition = defineWidget(
  WidgetInput.placeholderOrAstMatcher(Ast.BaseExpression),
  {
    priority: 40,
    score: (props) =>
      EnsoTypeExpression in props.input || props.input.expectedType === 'Type' ?
        Score.Perfect
      : Score.Mismatch,
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="input" />
</template>

<style scoped>
.EnsoWidgetTypeExpression {
  display: inline-flex;
  justify-content: center;
  align-items: center;
}
</style>
