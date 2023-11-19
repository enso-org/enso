<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import {
  Score,
  defineWidget,
  widgetAst,
  widgetExpression,
  type WidgetProps,
} from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { NumericLiteral, UnaryOprApp } from '@/util/ast/abstract'
import { computed } from 'vue'

const props = defineProps<WidgetProps>()
const graph = useGraphStore()
const value = computed({
  get() {
    return parseFloat(widgetExpression(props.input)?.code() ?? '')
  },
  set(value) {
    const id = widgetAst(props.input)?.exprId
    if (id) graph.setExpressionContent(id, value.toString())
  },
})
</script>
<script lang="ts">
export const widgetDefinition = defineWidget({
  priority: 10,
  match: (info) => {
    const ast = widgetAst(info.input)
    if (ast instanceof NumericLiteral) {
      return Score.Perfect
    } else if (
      ast instanceof UnaryOprApp &&
      ast.operator.code() === '-' &&
      ast.argument instanceof NumericLiteral
    ) {
      return Score.Perfect
    }
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
