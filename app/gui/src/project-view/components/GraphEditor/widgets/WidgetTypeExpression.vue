<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, widgetProps } from '@/providers/widgetRegistry'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
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
  EnsoTypeExpression,
  {
    priority: 40,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <NodeWidget :input="input" />
</template>
