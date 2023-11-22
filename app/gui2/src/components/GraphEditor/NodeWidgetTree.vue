<script setup lang="ts">
import { provideWidgetTree } from '@/providers/widgetTree'
import { useTransitioning } from '@/util/animation'
import type { AstExtended } from '@/util/ast'
import { toRef } from 'vue'
import NodeWidget from './NodeWidget.vue'

const props = defineProps<{ ast: AstExtended }>()

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
    <NodeWidget :input="ast" />
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
