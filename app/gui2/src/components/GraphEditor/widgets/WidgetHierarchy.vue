<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.typeName())
const children = computed(() => [...props.input.children()])
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(Ast.Ast, {
  priority: 1001,
})
</script>

<template>
  <div class="WidgetHierarchy" :class="spanClass">
    <NodeWidget v-for="(child, index) in children" :key="child.exprId ?? index" :input="child" />
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
