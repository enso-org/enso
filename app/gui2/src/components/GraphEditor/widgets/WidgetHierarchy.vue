<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.ast.typeName())
const children = computed(() => [...props.input.ast.children()])

function transformChild(child: Ast.Ast | Ast.Token) {
  const childInput = WidgetInput.FromAst(child)
  if (props.input.ast instanceof Ast.PropertyAccess && child === props.input.ast.lhs)
    childInput.forcePort = true
  if (
    props.input.ast instanceof Ast.OprApp &&
    (child === props.input.ast.rhs || child === props.input.ast.lhs)
  )
    childInput.forcePort = true
  if (props.input.ast instanceof Ast.UnaryOprApp && child === props.input.ast.argument)
    childInput.forcePort = true
  return childInput
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAst, {
  priority: 1001,
})
</script>

<template>
  <div class="WidgetHierarchy" :class="spanClass">
    <NodeWidget
      v-for="(child, index) in children"
      :key="child.exprId ?? index"
      :input="transformChild(child)"
    />
  </div>
</template>

<style scoped>
.WidgetHierarchy {
  display: flex;
  flex-direction: row;
  align-items: center;
  transition: background 0.2s ease;

  &.Literal {
    font-weight: bold;
  }
}
</style>
