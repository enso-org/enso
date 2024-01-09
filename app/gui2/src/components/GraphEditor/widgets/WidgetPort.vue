<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useRaf } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectPortInfo, providePortInfo, type PortId } from '@/providers/portInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { PortViewInstance, useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { Rect } from '@/util/data/rect'
import { cachedGetter } from '@/util/reactivity'
import { uuidv4 } from 'lib0/random'
import type { ExprId } from 'shared/yjsModel'
import {
  computed,
  markRaw,
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

const hasConnection = computed(
  () => graph.db.connections.reverseLookup(portId.value as ExprId).size > 0,
)
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
const portRect = shallowRef<Rect>()

const randomUuid = uuidv4() as PortId
// Since the port ID computation has many dependencies but rarely changes its final output, store
// its result in an intermediate ref, and update it only when the value actually changes. That way
// effects depending on the port ID value will not be re-triggered unnecessarily.
const portId = cachedGetter<PortId>(() => {
  return props.input.portId
})

const innerWidget = computed(() => {
  return { ...props.input, forcePort: false }
})

providePortInfo(proxyRefs({ portId, connected: hasConnection }))

watch(nodeSize, updateRect)
onUpdated(() => nextTick(updateRect))
useRaf(toRef(tree, 'hasActiveAnimations'), updateRect)

const randSlice = randomUuid.slice(0, 4)

watchEffect(
  (onCleanup) => {
    const id = portId.value
    const instance = markRaw(new PortViewInstance(portRect, tree.nodeId, props.onUpdate))
    graph.addPortInstance(id, instance)
    onCleanup(() => graph.removePortInstance(id, instance))
  },
  { flush: 'post' },
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
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 0,
  score: (props, _db) => {
    const portInfo = injectPortInfo(true)
    const value = props.input.value
    if (portInfo != null && value instanceof Ast.Ast && portInfo.portId === value.exprId) {
      return Score.Mismatch
    }

    if (
      props.input.forcePort ||
      WidgetInput.isPlaceholder(props.input) ||
      props.input[ArgumentInfoKey] != undefined
    )
      return Score.Perfect

    if (
      props.input.value instanceof Ast.Invalid ||
      props.input.value instanceof Ast.BodyBlock ||
      props.input.value instanceof Ast.Group ||
      props.input.value instanceof Ast.NumericLiteral ||
      props.input.value instanceof Ast.OprApp ||
      props.input.value instanceof Ast.UnaryOprApp ||
      props.input.value instanceof Ast.Wildcard ||
      props.input.value instanceof Ast.TextLiteral
    )
      return Score.Perfect

    return Score.Mismatch
  },
})
</script>

<template>
  <div
    ref="rootNode"
    class="WidgetPort"
    :class="{
      connected,
      'r-24': connected,
      newToConnect: !hasConnection && isCurrentEdgeHoverTarget,
      primary: props.nesting < 2,
    }"
    :data-id="portId"
    :data-h="randSlice"
    @pointerenter="isHovered = true"
    @pointerleave="isHovered = false"
  >
    <NodeWidget :input="innerWidget" />
  </div>
</template>

<style scoped>
:global(:root) {
  --widget-port-extra-pad: 6px;
}

.WidgetPort {
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: center;
  position: relative;
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
