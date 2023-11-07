<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Tree } from '@/generated/ast'
import { Score, defineWidget } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast, type AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps<{ ast: AstExtended }>()
const graph = useGraphStore()
const value = computed({
  get() {
    return props.ast.repr().endsWith('True')
  },
  set(value) {
    const node = getRawBoolNode(props.ast)
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
    candidate.isTree(Ast.Tree.Type.Ident) &&
    ['True', 'False'].includes(candidate.repr())
  ) {
    return candidate
  }
  return null
}

export const widgetConfig = defineWidget({
  beforeOverride: false,
  priority: 10,
  match: (info) => {
    if (getRawBoolNode(info.ast) != null) {
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
