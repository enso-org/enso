<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useTransitioning } from '@/composables/animation'
import { WidgetInput, type WidgetUpdate } from '@/providers/widgetRegistry'
import { provideWidgetTree } from '@/providers/widgetTree'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { Ast } from '@/util/ast'
import type { Icon } from '@/util/iconName'
import { computed, toRef } from 'vue'

const props = defineProps<{
  ast: Ast.Ast
  nodeId: NodeId
  icon: Icon
  connectedSelfArgumentId: Ast.AstId | undefined
  potentialSelfArgumentId: Ast.AstId | undefined
  conditionalPorts: Set<Ast.AstId>
  extended: boolean
}>()
const emit = defineEmits<{
  openFullMenu: []
  targetablePortsChanged: []
}>()
const graph = useGraphStore()
const rootPort = computed(() => {
  const input = WidgetInput.FromAst(props.ast)
  if (props.ast instanceof Ast.Ident && !graph.db.isKnownFunctionCall(props.ast.id)) {
    input.forcePort = true
  }
  return input
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

function handleWidgetUpdates(update: WidgetUpdate) {
  const edit = update.edit ?? graph.startEdit()
  if (update.portUpdate) {
    const { value, origin } = update.portUpdate
    if (Ast.isAstId(origin)) {
      const ast =
        value instanceof Ast.Ast ? value
        : value == null ? Ast.Wildcard.new(edit)
        : undefined
      if (ast) {
        edit.replaceValue(origin as Ast.AstId, ast)
      } else if (typeof value === 'string') {
        edit.tryGet(origin)?.syncToCode(value)
      }
    } else {
      console.error(`[UPDATE ${origin}] Invalid top-level origin. Expected expression ID.`)
    }
  }
  graph.commitEdit(edit)
  // This handler is guaranteed to be the last handler in the chain.
  return true
}

const layoutTransitions = useTransitioning(observedLayoutTransitions)
provideWidgetTree(
  toRef(props, 'ast'),
  toRef(props, 'nodeId'),
  toRef(props, 'icon'),
  toRef(props, 'connectedSelfArgumentId'),
  toRef(props, 'potentialSelfArgumentId'),
  toRef(props, 'conditionalPorts'),
  () => emit('targetablePortsChanged'),
  toRef(props, 'extended'),
  layoutTransitions.active,
  () => {
    emit('openFullMenu')
  },
)
</script>
<script lang="ts">
export const GRAB_HANDLE_X_MARGIN = 4
const GRAB_HANDLE_X_MARGIN_PX = `${GRAB_HANDLE_X_MARGIN}px`
export const ICON_WIDTH = 16
</script>

<template>
  <div class="NodeWidgetTree" spellcheck="false" v-on="layoutTransitions.events">
    <!-- Display an icon for the node if no widget in the tree provides one. -->
    <SvgIcon
      v-if="!props.connectedSelfArgumentId"
      class="icon grab-handle nodeCategoryIcon"
      :name="props.icon"
      @click.right.stop.prevent="emit('openFullMenu')"
    />
    <NodeWidget :input="rootPort" @update="handleWidgetUpdates" />
  </div>
</template>

<style scoped>
.NodeWidgetTree {
  color: white;

  outline: none;
  height: 24px;
  display: flex;
  align-items: center;

  &:has(.WidgetPort.newToConnect) {
    margin-left: calc(4px - var(--widget-port-extra-pad));
  }

  &:has(.WidgetPort.newToConnect > .r-24:only-child) {
    margin-left: 0px;
  }
}

.GraphEditor.draggingEdge .NodeWidgetTree {
  transition: margin 0.2s ease;
}

.icon {
  margin-right: 4px;
}

.grab-handle {
  color: white;
  margin: 0 v-bind('GRAB_HANDLE_X_MARGIN_PX');
}
</style>
