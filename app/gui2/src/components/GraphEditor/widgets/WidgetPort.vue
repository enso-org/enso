<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useRaf } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { ForcePort, injectPortInfo, providePortInfo } from '@/providers/portInfo'
import { AnyWidget, Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Rect } from '@/util/data/rect'
import { uuidv4 } from 'lib0/random'
import type { ExprId } from 'shared/yjsModel'
import {
  computed,
  nextTick,
  onUpdated,
  proxyRefs,
  ref,
  shallowRef,
  toRef,
  watch,
  watchEffect,
} from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()

const navigator = injectGraphNavigator()
const tree = injectWidgetTree()
const selection = injectGraphSelection(true)

const isHovered = ref(false)

const hasConnection = computed(() => graph.db.connections.reverseLookup(portId.value).size > 0)
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
const isRectUpdateUseful = computed(() => isHovered.value || hasConnection.value)

const randomUuid = uuidv4() as ExprId
const innerWidget = computed(() =>
  props.input instanceof ForcePort ? props.input.input : props.input,
)
const ast = computed(() => {
  const anyWidget = props.input instanceof ForcePort ? props.input.input : props.input
  return anyWidget.ast
})
const portId = computed(() => ast.value?.astId ?? randomUuid)

providePortInfo(proxyRefs({ portId, connected: hasConnection }))

watch(nodeSize, updateRect)
onUpdated(() => nextTick(updateRect))
useRaf(toRef(tree, 'hasActiveAnimations'), updateRect)

watch(
  () => [portId.value, portRect.value, isRectUpdateUseful.value] as const,
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
export const widgetDefinition = defineWidget([ForcePort, AnyWidget], {
  priority: 0,
  score: (props, _db) => {
    if (props.input instanceof ForcePort) return Score.Perfect

    const portInfo = injectPortInfo(true)
    if (portInfo != null && portInfo.portId === props.input.ast?.astId) {
      return Score.Mismatch
    }

    if (
      props.input.ast instanceof Ast.Invalid ||
      props.input.ast instanceof Ast.BodyBlock ||
      props.input.ast instanceof Ast.Group ||
      props.input.ast instanceof Ast.NumericLiteral ||
      props.input.ast instanceof Ast.OprApp ||
      props.input.ast instanceof Ast.UnaryOprApp ||
      props.input.ast instanceof Ast.Wildcard ||
      props.input.ast instanceof Ast.TextLiteral
    )
      return Score.Perfect

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
    ><NodeWidget :input="innerWidget"
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
  transition:
    margin 0.2s ease,
    padding 0.2s ease,
    background-color 0.2s ease;

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
