<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const value = computed({
  get() {
    return parseFloat(props.input.code())
  },
  set(value) {
    props.onUpdate(value.toString(), props.input.exprId)
  },
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  (input): input is Ast.NumericLiteral | Ast.NegationOprApp =>
    input instanceof Ast.NumericLiteral ||
    (input instanceof Ast.NegationOprApp && input.argument instanceof Ast.NumericLiteral),
  {
    priority: 10,
    score: Score.Perfect,
  },
)
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
