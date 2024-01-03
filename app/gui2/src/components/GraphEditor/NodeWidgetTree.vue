<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useTransitioning } from '@/composables/animation'
import { ForcePort, type PortId } from '@/providers/portInfo'
import { AnyWidget } from '@/providers/widgetRegistry'
import { provideWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { isUuid } from 'shared/yjsModel'
import { computed, toRef } from 'vue'

const props = defineProps<{ ast: Ast.Ast }>()
const graph = useGraphStore()
const rootPort = computed(() => {
  const input = AnyWidget.Ast(props.ast)
  return props.ast instanceof Ast.Ident && !graph.db.isKnownFunctionCall(props.ast.exprId)
    ? new ForcePort(input)
    : input
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

function handleWidgetUpdates(value: unknown, origin: PortId) {
  // TODO: Implement proper AST-based update.
  if (!isUuid(origin)) {
    console.error(`[UPDATE ${origin}] Invalid top-level origin. Expected expression ID.`)
  } else if (typeof value === 'string') {
    graph.setExpressionContent(origin, value)
  } else if (value instanceof Ast.Ast) {
    graph.setExpression(origin, value)
  } else if (value == null) {
    graph.setExpressionContent(origin, '_')
  } else {
    console.error(`[UPDATE ${origin}] Invalid value:`, value)
  }
  // No matter if its a succes or not, this handler is always considered to have handled the update,
  // since it is guaranteed to be the last handler in the chain.
  return true
}

const layoutTransitions = useTransitioning(observedLayoutTransitions)
provideWidgetTree(toRef(props, 'ast'), layoutTransitions.active)
</script>

<template>
  <div class="NodeWidgetTree" spellcheck="false" v-on="layoutTransitions.events">
    <NodeWidget :input="rootPort" @update="handleWidgetUpdates" />
  </div>
</template>

<style scoped>
.NodeWidgetTree {
  color: white;
  margin-left: 4px;

  outline: none;
  height: 24px;
  display: flex;
  align-items: center;

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
