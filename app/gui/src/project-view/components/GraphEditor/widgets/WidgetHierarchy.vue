<script setup lang="ts">
import { WidgetInput, defineWidget, widgetProps } from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Ast } from '@/util/ast'
import { computed } from 'vue'
import { isToken } from 'ydoc-shared/ast'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.value.typeName)

function* expressionChildren(expression: Ast.Expression) {
  for (const child of expression.children()) {
    if (isToken(child) || child.isExpression()) {
      yield child
    } else {
      console.error('Unable to render non-expression AST node in component', child)
    }
  }
}

function transformChild(child: Ast.Expression | Ast.Token) {
  const childInput = WidgetInput.FromAst(child)
  if (props.input.value instanceof Ast.PropertyAccess && child.id === props.input.value.lhs?.id)
    childInput.forcePort = true
  if (
    props.input.value instanceof Ast.OprApp &&
    (child.id === props.input.value.rhs?.id || child.id === props.input.value.lhs?.id)
  )
    childInput.forcePort = true
  if (props.input.value instanceof Ast.UnaryOprApp && child.id === props.input.value.argument?.id)
    childInput.forcePort = true
  return childInput
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAst,
  {
    priority: 1000,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetHierarchy widgetParent" :class="spanClass">
    <NodeWidget
      v-for="child in expressionChildren(props.input.value)"
      :key="child.id"
      :input="transformChild(child)"
    />
  </div>
</template>

<style scoped>
.WidgetHierarchy {
  transition: background 0.2s ease;

  &.Literal {
    font-weight: bold;
  }
}
</style>
