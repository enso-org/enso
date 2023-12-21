<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { AnyWidget, Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const value = computed({
  get() {
    return parseFloat(props.input.ast?.code() ?? '')
  },
  set(value) {
    const id = props.input.ast?.astId
    if (id) graph.setExpressionContent(id, value.toString())
    // TODO[ao] set number on placeholder.
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
    if (props.input.argInfo?.type === 'Standard.Base.Number') return Score.Perfect
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
