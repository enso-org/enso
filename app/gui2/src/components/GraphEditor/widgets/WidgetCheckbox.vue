<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const value = computed({
  get() {
    return WidgetInput.valueRepr(props.input)?.endsWith('True') ?? false
  },
  set(value) {
    if (props.input.value instanceof Ast.Ast) {
      const node = getRawBoolNode(props.input.value)
      if (node != null) {
        props.onUpdate({ type: 'set', value: value ? 'True' : 'False', origin: node.exprId })
      }
    } else {
      props.onUpdate({
        type: 'set',
        value: value ? 'Boolean.True' : 'Boolean.False',
        origin: props.input.portId,
      })
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

export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 1001,
  score: (props) => {
    if (props.input.value instanceof Ast.Ast && getRawBoolNode(props.input.value) != null)
      return Score.Perfect
    return props.input.expectedType === 'Standard.Base.Data.Boolean.Boolean'
      ? Score.Good
      : Score.Mismatch
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
