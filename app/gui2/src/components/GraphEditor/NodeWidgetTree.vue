<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useTransitioning } from '@/composables/animation'
import { injectGraphSelection } from '@/providers/graphSelection'
import { WidgetInput, type WidgetUpdate } from '@/providers/widgetRegistry'
import { provideWidgetTree } from '@/providers/widgetTree'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { Ast } from '@/util/ast'
import type { Vec2 } from '@/util/data/vec2'
import type { Icon } from '@/util/iconName'
import { computed, toRef, watch } from 'vue'

const props = defineProps<{
  ast: Ast.Ast
  nodeId: NodeId
  nodeElement: HTMLElement | undefined
  nodeSize: Vec2
  icon: Icon
  potentialSelfArgumentId: Ast.AstId | undefined
  /** Ports that are not targetable by default; see {@link NodeDataFromAst}. */
  conditionalPorts: Set<Ast.AstId>
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
const selection = injectGraphSelection()

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

function selectNode() {
  selection.setSelection(new Set([props.nodeId]))
}

function handleWidgetUpdates(update: WidgetUpdate) {
  selectNode()
  const edit = update.edit ?? graph.startEdit()
  if (update.portUpdate) {
    const { value, origin } = update.portUpdate
    if (Ast.isAstId(origin)) {
      const ast =
        value instanceof Ast.Ast ? value
        : value == null ? Ast.Wildcard.new(edit)
        : undefined
      if (ast) {
        edit.replaceValue(origin, ast)
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
const widgetTree = provideWidgetTree(
  toRef(props, 'ast'),
  toRef(props, 'nodeId'),
  toRef(props, 'nodeElement'),
  toRef(props, 'nodeSize'),
  toRef(props, 'icon'),
  toRef(props, 'potentialSelfArgumentId'),
  toRef(props, 'conditionalPorts'),
  toRef(props, 'extended'),
  layoutTransitions.active,
  () => emit('openFullMenu'),
)

watch(toRef(widgetTree, 'currentEdit'), (edit) => edit && selectNode())
</script>
<script lang="ts">
export const GRAB_HANDLE_X_MARGIN_L = 4
export const GRAB_HANDLE_X_MARGIN_R = 8
export const ICON_WIDTH = 16
</script>

<template>
  <div
    class="NodeWidgetTree NodeWidget RoundedWidget"
    spellcheck="false"
    v-on="layoutTransitions.events"
  >
    <!-- Display an icon for the node if no widget in the tree provides one. -->
    <SvgIcon
      v-if="!props.potentialSelfArgumentId"
      class="icon grab-handle nodeCategoryIcon draggable"
      :style="{ margin: `0 ${GRAB_HANDLE_X_MARGIN_R}px 0 ${GRAB_HANDLE_X_MARGIN_L}px` }"
      :name="props.icon"
      @click.right.stop.prevent="emit('openFullMenu')"
    />
    <NodeWidget :input="rootPort" @update="handleWidgetUpdates" />
  </div>
</template>

<style scoped>
.NodeWidgetTree {
  color: white;

  --widget-token-pad-unit: 6px;

  outline: none;
  min-height: 24px;
  display: flex;
  align-items: center;

  --token-pad-left: var(--widget-token-pad-unit);
  --token-pad-right: var(--widget-token-pad-unit);
}

.NodeWidgetTree {
  *:not(:nth-child(1 of :not(.OutOfLayout))) {
    --token-pad-left: 0px;
  }
  *:not(:nth-last-child(1 of :not(.OutOfLayout))) {
    --token-pad-right: 0px;
  }

  :deep(.RoundedWidget.RoundedWidget) {
    --token-pad-left: var(--widget-token-pad-unit);
    --token-pad-right: var(--widget-token-pad-unit);
  }

  :deep(.NoTokenPadding.NoTokenPadding) {
    --token-pad-left: 0px;
    --token-pad-right: 0px;
  }

  :deep(.TokenPadding) {
    padding-left: var(--token-pad-left, 0);
    padding-right: var(--token-pad-right, 0);
    transition: padding 0.2s;
  }
}

.GraphEditor.draggingEdge .NodeWidgetTree {
  transition: margin 0.2s ease;
}
</style>
