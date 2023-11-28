<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.treeTypeName())
const children = computed(() => [...props.input.children()])
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(AstExtended.isTree(), {
  priority: 1001,
})
</script>

<template>
  <span :class="['Tree', spanClass]"
    ><NodeWidget v-for="(child, index) in children" :key="child.astId ?? index" :input="child" />
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
