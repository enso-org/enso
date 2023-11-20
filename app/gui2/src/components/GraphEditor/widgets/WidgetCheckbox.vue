<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Score, defineWidget, widgetAst, type WidgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import type { Ast } from '@/util/ast/abstract'
import { PropertyAccess } from '@/util/ast/abstract.ts'
import { computed } from 'vue'

const props = defineProps<WidgetProps>()
const graph = useGraphStore()
const value = computed({
  get() {
    return widgetAst(props.input)?.code().endsWith('True') ?? false
  },
  set(value) {
    const ast = widgetAst(props.input)
    const node = ast && getRawBoolNode(ast)
    if (node != null) {
      graph.setExpressionContent(node.exprId, value ? 'True' : 'False')
    }
  },
})
</script>
<script lang="ts">
function getRawBoolNode(ast: Ast) {
  const candidate =
    ast instanceof PropertyAccess && ast.code().startsWith('Boolean.')
      ? ast.tryMap((t) => t.rhs)
      : ast
  if (
    candidate &&
    candidate.isTree(Ast.Tree.Type.Ident) &&
    ['True', 'False'].includes(candidate.code())
  ) {
    return candidate
  }
  return null
}

export const widgetDefinition = defineWidget({
  priority: 10,
  match: (info) => {
    const ast = widgetAst(info.input)
    if (ast && getRawBoolNode(ast) != null) {
      return Score.Perfect
    }
    return Score.Mismatch
  },
})
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
