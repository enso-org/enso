<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { Tree } from '@/generated/ast'
import { Score, defineWidget, widgetAst, type WidgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { computed } from 'vue'

const props = defineProps<WidgetProps>()
const graph = useGraphStore()
const value = computed({
  get() {
    return parseFloat(widgetAst(props.input)?.repr() ?? '')
  },
  set(value) {
    const id = widgetAst(props.input)?.astId
    if (id) graph.setExpressionContent(id, value.toString())
  },
})
</script>
<script lang="ts">
export const widgetDefinition = defineWidget({
  priority: 10,
  match: (info) => {
    const ast = widgetAst(info.input)
    if (!ast) return Score.Mismatch
    if (ast.isTree(Tree.Type.UnaryOprApp)) {
      if (
        ast.map((t) => t.opr).repr() === '-' &&
        ast.tryMap((t) => t.rhs)?.isTree(Tree.Type.Number)
      ) {
        return Score.Perfect
      }
    } else if (ast.isTree(Tree.Type.Number)) {
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
