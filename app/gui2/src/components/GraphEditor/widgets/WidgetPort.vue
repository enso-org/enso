<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import {
  Score,
  defineWidget,
  widgetAst,
  type WidgetInput,
  type WidgetProps,
} from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import type { GraphDb } from '@/stores/graph/graphDatabase'
import { useRaf } from '@/util/animation'
import { Ast } from '@/util/ast'
import { useResizeObserver } from '@/util/events'
import { Rect } from '@/util/rect'
import { uuidv4 } from 'lib0/random'
import type { ExprId } from 'shared/yjsModel'
import { computed, nextTick, onUpdated, ref, shallowRef, toRef, watch, watchEffect } from 'vue'

const graph = useGraphStore()
const props = defineProps<WidgetProps>()
const navigator = injectGraphNavigator()
const tree = injectWidgetTree()
const selection = injectGraphSelection(true)

const isHovered = ref(false)
const hasConnection = computed(() => isConnected(props.input, graph.db))
const isCurrentEdgeHoverTarget = computed(
  () => isHovered.value && graph.unconnectedEdge != null && selection?.hoveredPort === portId.value,
)
const connected = computed(() => hasConnection.value || isCurrentEdgeHoverTarget.value)

const rootNode = shallowRef<HTMLElement>()
const nodeSize = useResizeObserver(rootNode, false)

watchEffect((onCleanup) => {
  if (selection != null && isHovered.value === true) {
    const id = portId.value
    selection.addHoveredPort(id)
    onCleanup(() => selection.removeHoveredPort(id))
  }
})

// Compute the scene-space bounding rectangle of the expression's widget. Those bounds are later
// used for edge positioning. Querying and updating those bounds is relatively expensive, so we only
// do it when the node has any potential for being used as an edge source or target. This is true
// when any of following conditions are met:
// 1. The expression can be connected to and is currently being hovered.
// 2. The expression is already used as an existing edge endpoint.
//
// TODO: This should part of the `WidgetPort` component. But first, we need to make sure that the
// ports are always created when necessary.
const portRect = shallowRef<Rect>()
const rectUpdateIsUseful = computed(() => isHovered.value || hasConnection.value)

const randomUuid = uuidv4() as ExprId
const portId = computed(() => widgetAst(props.input)?.astId ?? randomUuid)

watch(nodeSize, updateRect)
onUpdated(() => {
  nextTick(updateRect)
})
useRaf(toRef(tree, 'hasActiveAnimations'), updateRect)

watch(
  () => [portId.value, portRect.value, rectUpdateIsUseful.value] as const,
  ([id, rect, updateUseful], _, onCleanup) => {
    if (id != null && rect != null && updateUseful) {
      graph.updateExprRect(id, rect)
      onCleanup(() => {
        if (portId.value === id && rect === portRect.value) graph.updateExprRect(id, undefined)
      })
    }
  },
)

function updateRect() {
  let domNode = rootNode.value
  const rootDomNode = domNode?.closest('.node')
  if (domNode == null || rootDomNode == null) return

  const exprClientRect = Rect.FromDomRect(domNode.getBoundingClientRect())
  const nodeClientRect = Rect.FromDomRect(rootDomNode.getBoundingClientRect())
  const exprSceneRect = navigator.clientToSceneRect(exprClientRect)
  const exprNodeRect = navigator.clientToSceneRect(nodeClientRect)
  const localRect = exprSceneRect.offsetBy(exprNodeRect.pos.inverse())
  if (portRect.value != null && localRect.equals(portRect.value)) return
  portRect.value = localRect
}
</script>

<script lang="ts">
function canBeConnectedTo(input: WidgetInput): boolean {
  const ast = widgetAst(input)
  if (ast == null) return true // placeholders are always connectable
  if (ast.isToken()) return false
  switch (ast.inner.type) {
    case Ast.Tree.Type.Invalid:
    case Ast.Tree.Type.BodyBlock:
    case Ast.Tree.Type.Ident:
    case Ast.Tree.Type.Group:
    case Ast.Tree.Type.Number:
    case Ast.Tree.Type.OprApp:
    case Ast.Tree.Type.UnaryOprApp:
    case Ast.Tree.Type.Wildcard:
    case Ast.Tree.Type.TextLiteral:
      return true
    default:
      return false
  }
}

function isConnected(input: WidgetInput, db: GraphDb) {
  const astId = widgetAst(input)?.astId
  return astId != null && db.connections.reverseLookup(astId).size > 0
}
export const widgetDefinition = defineWidget({
  priority: 1,
  match: (info) => {
    if (canBeConnectedTo(info.input)) {
      return Score.Perfect
    }
    return Score.Mismatch
  },
})
</script>
<template>
  <span
    ref="rootNode"
    class="WidgetPort"
    :class="{
      connected,
      'r-24': connected,
      newToConnect: !hasConnection && isCurrentEdgeHoverTarget,
      primary: props.nesting < 2,
    }"
    @pointerenter="isHovered = true"
    @pointerleave="isHovered = false"
    ><NodeWidget :input="props.input"
  /></span>
</template>

<style scoped>
:global(:root) {
  --widget-port-extra-pad: 6px;
}

.WidgetPort {
  display: inline-block;
  position: relative;
  vertical-align: middle;
  text-align: center;
  border-radius: 12px;
  min-height: 24px;
  min-width: 24px;
  box-sizing: border-box;
  padding: 0 var(--widget-port-extra-pad);
  margin: 0 calc(0px - var(--widget-port-extra-pad));
  transition:
    margin 0.2s ease,
    padding 0.2s ease,
    background-color 0.2s ease;
}

.WidgetPort:has(> .r-24:only-child) {
  padding: 0;
  margin: 0;
  transition: background-color 0.2s ease;
}

.WidgetPort.connected {
  margin: 0;
  background-color: var(--node-color-port);
}

.GraphEditor.draggingEdge .WidgetPort {
  pointer-events: none;

  &::before {
    pointer-events: all;
    content: '';
    position: absolute;
    display: block;
    inset: 4px var(--widget-port-extra-pad);
  }

  /* Expand hover area for primary ports. */
  &.primary::before {
    top: -4px;
    bottom: -4px;
  }

  &.connected::before {
    left: 0px;
    right: 0px;
  }
}
</style>
