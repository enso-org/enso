<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, defineWidget } from '@/providers/widgetRegistry'
import { Ast, type AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps<{ ast: AstExtended<Ast.Tree> }>()

const spanClass = computed(() => Ast.Tree.typeNames[props.ast.inner.type])
const children = computed(() => [...props.ast.children()])
</script>

<script lang="ts">
export const widgetConfig = defineWidget({
  beforeOverride: false,
  priority: 2,
  match: (info) => (info.ast.isTree() ? Score.Good : Score.Mismatch),
})
</script>

<template>
  <span :class="['Tree', spanClass]"
    ><NodeWidget v-for="child in children" :key="child.astId" :ast="child" />
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
