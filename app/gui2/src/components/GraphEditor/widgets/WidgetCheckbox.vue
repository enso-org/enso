<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()
const value = computed({
  get() {
    return props.input.code().endsWith('True') ?? false
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
function getRawBoolNode(ast: Ast.Ast) {
  const candidate =
    ast instanceof Ast.PropertyAccess && ast.lhs?.code() === 'Boolean' ? ast.rhs : ast
  if (candidate instanceof Ast.Ident && ['True', 'False'].includes(candidate.code())) {
    return candidate
  }
  return null
}

export const widgetDefinition = defineWidget(
  (input) => input instanceof Ast.PropertyAccess || input instanceof Ast.Ident,
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
