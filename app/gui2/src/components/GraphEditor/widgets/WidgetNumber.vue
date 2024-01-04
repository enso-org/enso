<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { AnyWidget, Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const value = computed({
  get() {
    const valueStr = props.input.ast?.code() ?? props.input.argInfo?.defaultValue ?? ''
    return valueStr ? parseFloat(valueStr) : 0
  },
  set(value) {
    props.onUpdate(value.toString(), props.input.portId)
  },
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(AnyWidget, {
  priority: 10,
  score: (props) => {
    if (
      props.input.ast instanceof Ast.NumericLiteral ||
      (props.input.ast instanceof Ast.NegationOprApp &&
        props.input.ast.argument instanceof Ast.NumericLiteral)
    )
      return Score.Perfect
    if (
      props.input.argInfo?.reprType === 'Standard.Base.Data.Number' ||
      props.input.argInfo?.reprType === 'Standard.Base.Data.Numbers.Integer' ||
      props.input.argInfo?.reprType === 'Standard.Data.Numbers.Float'
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
