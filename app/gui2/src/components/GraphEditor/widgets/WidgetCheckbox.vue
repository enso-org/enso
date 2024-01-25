<script setup lang="ts">
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import type { TokenId } from '@/util/ast/abstract.ts'
import { asNot } from '@/util/data/types.ts'
import { type Identifier, type QualifiedName } from '@/util/qualifiedName.ts'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const value = computed({
  get() {
    return WidgetInput.valueRepr(props.input)?.endsWith('True') ?? false
  },
  set(value) {
    const edit = graph.astModule.edit()
    if (props.input.value instanceof Ast.Ast) {
      const node = getRawBoolNode(props.input.value)
      if (node != null) {
        props.onUpdate({
          edit,
          portUpdate: { value: value ? 'True' : 'False', origin: node.id },
        })
      }
    } else {
      graph.addMissingImports(edit, [
        {
          kind: 'Unqualified',
          from: 'Standard.Base.Data.Boolean' as QualifiedName,
          import: 'Boolean' as Identifier,
        },
      ])
      props.onUpdate({
        edit,
        portUpdate: {
          value: value ? 'Boolean.True' : 'Boolean.False',
          origin: asNot<TokenId>(props.input.portId),
        },
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
