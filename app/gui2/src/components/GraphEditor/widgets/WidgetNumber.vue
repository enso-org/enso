<script setup lang="ts">
import NumericInputWidget from '@/components/widgets/NumericInputWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import type { TokenId } from '@/util/ast/abstract.ts'
import { asNot } from '@/util/data/types.ts'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const value = computed({
  get() {
    const valueStr = WidgetInput.valueRepr(props.input)
    return valueStr ? parseFloat(valueStr) : 0
  },
  set(value) {
    props.onUpdate({
      edit: graph.astModule.edit(),
      portUpdate: { value: value.toString(), origin: asNot<TokenId>(props.input.portId) },
    })
  },
})

const limits = computed(() => {
  const config = props.input.dynamicConfig
  if (config?.kind === 'Numeric_Input' && config?.minimum != null && config?.maximum != null) {
    return { min: config.minimum, max: config.maximum }
  } else {
    return undefined
  }
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 1001,
  score: (props) => {
    if (
      props.input.value instanceof Ast.NumericLiteral ||
      (props.input.value instanceof Ast.NegationOprApp &&
        props.input.value.argument instanceof Ast.NumericLiteral)
    )
      return Score.Perfect
    const type = props.input.expectedType
    if (
      type === 'Standard.Base.Data.Number' ||
      type === 'Standard.Base.Data.Numbers.Integer' ||
      type === 'Standard.Data.Numbers.Float'
    )
      return Score.Perfect
    return Score.Mismatch
  },
})
</script>

<template>
  <NumericInputWidget v-model="value" class="WidgetNumber r-24" :limits="limits" />
</template>

<style scoped>
.WidgetNumber {
  display: inline-block;
  vertical-align: middle;
}
</style>
