<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const value = computed({
  get() {
    const valueStr = WidgetInput.valueRepr(props.input)
    return valueStr ? parseFloat(valueStr) : 0
  },
  set(value) {
    props.onUpdate(value.toString(), props.input.portId)
  },
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
  <SliderWidget v-model="value" class="WidgetNumber r-24" :min="-1000" :max="1000" />
</template>

<style scoped>
.WidgetNumber {
  display: inline-block;
  vertical-align: middle;
}
</style>
