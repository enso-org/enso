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
  extended: boolean
}>()
const emit = defineEmits<{
  openFullMenu: []
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
        value instanceof Ast.Ast ? value : value == null ? Ast.Wildcard.new(edit) : undefined
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
  toRef(props, 'extended'),
  layoutTransitions.active,
  () => {
    emit('openFullMenu')
  },
)
</script>

<template>
  <div class="NodeWidgetTree" spellcheck="false" v-on="layoutTransitions.events">
    <!-- Display an icon for the node if no widget in the tree provides one. -->
    <SvgIcon
      v-if="!props.connectedSelfArgumentId"
      class="icon grab-handle"
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
    margin-left: 4px;
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
  margin: 0 4px;
}
</style>
