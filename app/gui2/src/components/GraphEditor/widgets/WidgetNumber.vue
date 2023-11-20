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
import { computed } from 'vue'
import { Ast } from "@/util/ast"

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
    const ast = info.input
    if (ast instanceof Ast.NumericLiteral) {
      return Score.Perfect
    } else if (
      ast instanceof Ast.NegationOprApp &&
      ast.argument instanceof Ast.NumericLiteral
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
