<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { ForcePort } from '@/providers/portInfo'
import { AnyWidget, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.ast.typeName())
const children = computed(() => [...props.input.ast.children()])

function transformChild(child: Ast.Ast | Ast.Token) {
  if (child instanceof Ast.Token) return child
  const childInput = AnyWidget.Ast(child)
  if (props.input.ast instanceof Ast.PropertyAccess) {
    if (child === props.input.ast.lhs) {
      return new ForcePort(childInput)
    }
  } else if (props.input.ast instanceof Ast.OprApp) {
    if (child === props.input.ast.rhs || child === props.input.ast.lhs) {
      return new ForcePort(childInput)
    }
  } else if (props.input.ast instanceof Ast.UnaryOprApp && child === props.input.ast.argument) {
    return new ForcePort(childInput)
  }
  return childInput
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(AnyWidget.matchAst, {
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
