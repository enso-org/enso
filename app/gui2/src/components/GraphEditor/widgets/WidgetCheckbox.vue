<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { AnyWidget, Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const value = computed({
  get() {
    return props.input.ast?.code().endsWith('True') ?? false
  },
  set(value) {
    if (props.input.ast == null) return // TODO[ao] set value on placeholder here.
    const node = getRawBoolNode(props.input.ast)
    if (node != null) {
      props.onUpdate(value ? 'True' : 'False', node.exprId)
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

export const widgetDefinition = defineWidget(AnyWidget, {
  priority: 10,
  score: (props) => {
    if (props.input.ast == null)
      return props.input.argInfo?.reprType === 'Standard.Base.Bool' ? Score.Good : Score.Mismatch
    if (getRawBoolNode(props.input.ast) != null) return Score.Perfect
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
