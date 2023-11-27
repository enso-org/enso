<script setup lang="ts">
import { Tree } from '@/generated/ast'
import { ForcePort } from '@/providers/portInfo'
import { provideWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { useTransitioning } from '@/util/animation'
import type { AstExtended } from '@/util/ast'
import { computed, toRef } from 'vue'
import NodeWidget from './NodeWidget.vue'

const props = defineProps<{ ast: AstExtended }>()
const graph = useGraphStore()
const rootPort = computed(() => {
  return props.ast.isTree(Tree.Type.Ident) && !graph.db.isKnownFunctionCall(props.ast.astId)
    ? new ForcePort(props.ast)
    : props.ast
})

const observedLayoutTransitions = new Set([
  'margin-left',
  'margin-right',
  'margin-top',
  'margin-bottom',
  'padding-left',
  'padding-right',
  'padding-top',
  'padding-bottom',
  'width',
  'height',
])

const layoutTransitions = useTransitioning(observedLayoutTransitions)
provideWidgetTree(toRef(props, 'ast'), layoutTransitions.active)
</script>

<template>
  <span class="NodeWidgetTree" spellcheck="false" v-on="layoutTransitions.events">
    <NodeWidget :input="rootPort" />
  </span>
</template>

<style scoped>
.NodeWidgetTree {
  color: white;
  margin-left: 4px;

  outline: none;
  height: 24px;
  display: inline-flex;
  align-items: center;

  & :deep(span) {
    vertical-align: middle;
  }

  &:has(.WidgetPort.newToConnect) {
    margin-left: calc(4px - var(--widget-port-extra-pad));
  }

  &:has(.WidgetPort.newToConnect > .r-24:only-child) {
    margin-left: 4px;
  }
}

.GraphEditor.draggingEdge .NodeWidgetTree {
  transition: margin 0.2s ease;
}
</style>
