<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { RawAst, RawAstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const value = computed({
  get() {
    return parseFloat(props.input.repr() ?? '')
  },
  set(value) {
    const id = props.input.astId
    if (id) graph.setExpressionContent(id, value.toString())
  },
})
</script>
<script lang="ts">
export const widgetDefinition = defineWidget(
  RawAstExtended.isTree([RawAst.Tree.Type.UnaryOprApp, RawAst.Tree.Type.Number]),
  {
    priority: 10,
    score: (props) => {
      if (props.input.isTree(RawAst.Tree.Type.UnaryOprApp)) {
        if (
          props.input.map((t) => t.opr).repr() === '-' &&
          props.input.tryMap((t) => t.rhs)?.isTree(RawAst.Tree.Type.Number)
        ) {
          return Score.Perfect
        }
      } else if (props.input.isTree(RawAst.Tree.Type.Number)) {
        return Score.Perfect
      }
      return Score.Mismatch
    },
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
