<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { RawAst, RawAstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()
const value = computed({
  get() {
    return props.input.repr().endsWith('True') ?? false
  },
  set(value) {
    const node = getRawBoolNode(props.input)
    if (node != null) {
      graph.setExpressionContent(node.astId, value ? 'True' : 'False')
    }
  },
})
</script>
<script lang="ts">
function getRawBoolNode(ast: RawAstExtended) {
  const candidate =
    ast.isTree(RawAst.Tree.Type.OprApp) && ast.repr().startsWith('Boolean.')
      ? ast.tryMap((t) => t.rhs)
      : ast
  if (
    candidate &&
    candidate.isTree(RawAst.Tree.Type.Ident) &&
    ['True', 'False'].includes(candidate.repr())
  ) {
    return candidate
  }
  return null
}

export const widgetDefinition = defineWidget(
  RawAstExtended.isTree([RawAst.Tree.Type.OprApp, RawAst.Tree.Type.Ident]),
  {
    priority: 10,
    score: (props) => {
      if (getRawBoolNode(props.input) != null) {
        return Score.Perfect
      }
      return Score.Mismatch
    },
  },
)
</script>
<template>
  <CheckboxWidget
    v-model="value"
    class="WidgetCheckbox"
    contenteditable="false"
    @beforeinput.stop
  />
</template>

<style scoped></style>
