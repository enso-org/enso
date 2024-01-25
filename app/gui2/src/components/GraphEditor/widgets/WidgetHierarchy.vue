<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.value.typeName())
const children = computed(() => [...props.input.value.children()])

function transformChild(child: Ast.Ast | Ast.Token) {
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
export const widgetDefinition = defineWidget(WidgetInput.isAst, {
  priority: 2000,
})
</script>

<template>
  <div class="WidgetHierarchy" :class="spanClass">
    <NodeWidget
      v-for="(child, index) in children"
      :key="child.id ?? index"
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
