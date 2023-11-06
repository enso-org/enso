<script setup lang="ts">
import SliderWidget from '@/components/widgets/SliderWidget.vue'
import { Tree } from '@/generated/ast'
import { Score, defineWidget } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { type AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps<{ ast: AstExtended }>()
const graph = useGraphStore()
const value = computed({
  get() {
    return parseFloat(props.ast.repr())
  },
  set(value) {
    graph.setExpressionContent(props.ast.astId, value.toString())
  },
})
</script>
<script lang="ts">
export const widgetConfig = defineWidget({
  beforeOverride: false,
  priority: 10,
  match: (info) => {
    if (info.ast.isTree(Tree.Type.UnaryOprApp)) {
      if (
        info.ast.map((t) => t.opr).repr() === '-' &&
        info.ast.tryMap((t) => t.rhs)?.isTree(Tree.Type.Number)
      ) {
        return Score.Perfect
      }
    } else if (info.ast.isTree(Tree.Type.Number)) {
      return Score.Perfect
    }
    return Score.Mismatch
  },
})
</script>
<template>
  <SliderWidget
    v-model="value"
    class="WidgetNumber"
    :min="-1000"
    :max="1000"
    contenteditable="false"
    @beforeinput.stop
  />
</template>

<style scoped>
.WidgetNumber {
  display: inline-block;
  vertical-align: middle;
}
</style>
