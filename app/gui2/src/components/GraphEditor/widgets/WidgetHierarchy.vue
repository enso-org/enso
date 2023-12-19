<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { ForcePort } from '@/providers/portInfo'
import type { WidgetInput } from '@/providers/widgetRegistry'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.typeName())
const children = computed(() => [...props.input.children()])

function transformChild(child: WidgetInput) {
  if (!(props.input instanceof Ast.Ast)) return child
  if (props.input instanceof Ast.PropertyAccess) {
    if (child === props.input.lhs) {
      return new ForcePort(child)
    }
  } else if (props.input instanceof Ast.OprApp) {
    if (child === props.input.rhs || child === props.input.lhs) {
      return new ForcePort(child)
    }
  } else if (props.input instanceof Ast.UnaryOprApp && child === props.input.argument) {
    return new ForcePort(child)
  }
  return child
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget((expression) => expression instanceof Ast.Ast, {
  priority: 1001,
})
</script>

<template>
  <span :class="['Tree', spanClass]"
    ><NodeWidget
      v-for="(child, index) in children"
      :key="child.astId ?? index"
      :input="transformChild(child)"
    />
  </span>
</template>

<style scoped>
.Tree {
  white-space: pre;
  align-items: center;
  transition: background 0.2s ease;
  min-height: 24px;
  display: inline-block;

  &.Literal {
    font-weight: bold;
  }

  &.port {
    background-color: var(--node-color-port);
    border-radius: var(--node-border-radius);
    margin: -2px -4px;
    padding: 2px 4px;
  }
}
</style>
