<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Tree } from '@/generated/ast'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { AstExtended } from '@/util/ast'
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
function getRawBoolNode(ast: AstExtended) {
  const candidate =
    ast.isTree(Tree.Type.OprApp) && ast.repr().startsWith('Boolean.')
      ? ast.tryMap((t) => t.rhs)
      : ast
  if (
    candidate &&
    candidate.isTree(Tree.Type.Ident) &&
    ['True', 'False'].includes(candidate.repr())
  ) {
    return candidate
  }
  return null
}

export const widgetDefinition = defineWidget(
  AstExtended.isTree([Tree.Type.OprApp, Tree.Type.Ident]),
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
