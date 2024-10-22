<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useRaf } from '@/composables/animation'
import { useResizeObserver } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectKeyboard } from '@/providers/keyboard'
import { injectPortInfo, providePortInfo, type PortId } from '@/providers/portInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { PortViewInstance, useGraphStore } from '@/stores/graph'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { Rect } from '@/util/data/rect'
import { cachedGetter } from '@/util/reactivity'
import {
  computed,
  nextTick,
  onMounted,
  onUpdated,
  proxyRefs,
  shallowRef,
  toRef,
  watch,
  watchEffect,
} from 'vue'
import { isUuid } from 'ydoc-shared/yjsModel'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()

const navigator = injectGraphNavigator()
const tree = injectWidgetTree()
const selection = injectGraphSelection(true)

const hasConnection = computed(() => graph.isConnectedTarget(portId.value))
const isCurrentEdgeHoverTarget = computed(
  () =>
    graph.mouseEditedEdge?.source != null &&
    selection?.hoveredPort === portId.value &&
    graph.db.getPatternExpressionNodeId(graph.mouseEditedEdge.source) !== tree.nodeId,
)
const isCurrentDisconnectedEdgeTarget = computed(
  () =>
    graph.mouseEditedEdge?.disconnectedEdgeTarget === portId.value &&
    graph.mouseEditedEdge?.target !== portId.value,
)
const isSelfArgument = computed(
  () =>
    props.input.value instanceof Ast.Ast && props.input.value.id === tree.potentialSelfArgumentId,
)
const connected = computed(
  () => (!isSelfArgument.value && hasConnection.value) || isCurrentEdgeHoverTarget.value,
)
const isTarget = computed(
  () =>
    (hasConnection.value && !isCurrentDisconnectedEdgeTarget.value) ||
    isCurrentEdgeHoverTarget.value,
)

const rootNode = shallowRef<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)

// Compute the scene-space bounding rectangle of the expression's widget. Those bounds are later
// used for edge positioning. Querying and updating those bounds is relatively expensive, so we only
// do it when the node has any potential for being used as an edge source or target. This is true
// when any of following conditions are met:
// 1. The expression can be connected to and is currently being hovered.
// 2. The expression is already used as an existing edge endpoint.
const portRect = shallowRef<Rect>()

// Since the port ID computation has many dependencies but rarely changes its final output, store
// its result in an intermediate ref, and update it only when the value actually changes. That way
// effects depending on the port ID value will not be re-triggered unnecessarily.
const portId = cachedGetter<PortId>(() => {
  assert(!isUuid(props.input.portId))
  return props.input.portId
})

const innerWidget = computed(() => {
  return { ...props.input, forcePort: false }
})

providePortInfo(proxyRefs({ portId, connected: hasConnection }))

watchEffect(
  (onCleanup) => {
    const id = portId.value
    const instance = new PortViewInstance(portRect, tree.nodeId, props.onUpdate)
    graph.addPortInstance(id, instance)
    onCleanup(() => graph.removePortInstance(id, instance))
  },
  { flush: 'post' },
)

const keyboard = injectKeyboard()

const enabled = computed(() => {
  const input = props.input.value
  const isConditional = input instanceof Ast.Ast && tree.conditionalPorts.has(input.id)
  return !isConditional || keyboard.mod
})

/**
 * NOTE: Reactive dependencies of this function are enforced externally in a `watch` below. This is
 * necessary, since we don't want to introduce very noisy dependencies through `clientToSceneRect`
 * call. Since this function calls `getBoundingClientRect`, it can't automatically track all its
 * dependencies anyway and external refresh mechanisms are required.
 */
function updateRect() {
  const oldRect = portRect.value
  const newRect = relativePortSceneRect()
  if (
    oldRect !== newRect &&
    (oldRect == null || newRect == null || !oldRect.equalsApproximately(newRect, 0.01))
  ) {
    portRect.value = newRect
  }
}

function relativePortSceneRect(): Rect | undefined {
  const domNode = rootNode.value
  const rootDomNode = tree.nodeElement
  if (domNode == null || rootDomNode == null) return
  if (!enabled.value) return
  const exprClientRect = Rect.FromDomRect(domNode.getBoundingClientRect())
  const nodeClientRect = Rect.FromDomRect(rootDomNode.getBoundingClientRect())
  const exprSceneRect = navigator.clientToSceneRect(exprClientRect)
  const exprNodeRect = navigator.clientToSceneRect(nodeClientRect)
  const rect = exprSceneRect.offsetBy(exprNodeRect.pos.inverse())
  return rect.isFinite() ? rect : undefined
}

watch(
  () => [nodeSize.value, rootNode.value, tree.nodeElement, tree.nodeSize, enabled.value],
  updateRect,
)
onUpdated(() => nextTick(updateRect))
onMounted(() => nextTick(updateRect))
useRaf(toRef(tree, 'hasActiveAnimations'), updateRect)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 0,
    score: (props, _db) => {
      const portInfo = injectPortInfo(true)
      const value = props.input.value
      if (portInfo != null && value instanceof Ast.Ast && portInfo.portId === value.id) {
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
        props.input.value instanceof Ast.PropertyAccess ||
        props.input.value instanceof Ast.UnaryOprApp ||
        props.input.value instanceof Ast.Wildcard ||
        props.input.value instanceof Ast.TextLiteral
      )
        return Score.Perfect

      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div
    ref="rootNode"
    class="WidgetPort"
    :class="{
      enabled,
      connected,
      isTarget,
      isSelfArgument,
      widgetRounded: connected,
      newToConnect: !hasConnection && isCurrentEdgeHoverTarget,
      primary: props.nesting < 2,
    }"
  >
    <NodeWidget :input="innerWidget" />
  </div>
</template>

<style scoped>
.WidgetPort {
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: center;
  position: relative;
  text-align: center;
  border-radius: var(--node-port-border-radius);
  min-height: var(--node-port-height);
  min-width: var(--node-port-height);
  box-sizing: border-box;
}

.WidgetPort.connected {
  background-color: var(--node-color-port);
}

.GraphEditor.draggingEdge .WidgetPort {
  --node-port-nonprimary-drag-shrink: 8px;
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
    inset: calc(
        (var(--node-port-height) - var(--node-base-height)) / 2 +
          var(--node-port-nonprimary-drag-shrink)
      )
      var(--widget-token-pad-unit);
  }

  /* Expand hover area for primary ports. */
  &.primary::before {
    inset: calc((var(--node-port-height) - var(--node-base-height)) / 2)
      var(--widget-token-pad-unit);
  }

  &.connected::before {
    left: 0px;
    right: 0px;
  }
}

.WidgetPort.isTarget:not(.isSelfArgument):after {
  content: '';
  position: absolute;
  top: -4px;
  left: 50%;
  width: 4px;
  height: 5px;
  transform: translate(-50%, 0);
  background-color: var(--node-color-port);
  z-index: -1;
}
</style>
