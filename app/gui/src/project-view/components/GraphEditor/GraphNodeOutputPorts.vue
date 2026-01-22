<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import type { NodeId } from '$/providers/openedProjects/graph'
import { setsIntersect } from '$/utils/data/set'
import CreateNodeFromPortButton from '@/components/GraphEditor/CreateNodeFromPortButton.vue'
import { useApproach } from '@/composables/animation'
import { useComponentColors } from '@/composables/componentColors'
import { useDoubleClick } from '@/composables/doubleClick'
import { useGraphEditorState } from '@/providers/graphEditorState'
import { useGraphSelection } from '@/providers/graphSelection'
import { isDef } from '@vueuse/core'
import { setIfUndefined } from 'lib0/map'
import {
  computed,
  effectScope,
  onScopeDispose,
  ref,
  toRef,
  watch,
  watchEffect,
  type EffectScope,
} from 'vue'
import type { AstId } from 'ydoc-shared/ast'

const props = defineProps<{ nodeId: NodeId }>()

const emit = defineEmits<{
  portClick: [event: PointerEvent, portId: AstId]
  portDoubleClick: [event: PointerEvent, portId: AstId]
  newNodeClick: [portId: AstId]
  newNodeDrag: [portId: AstId]
}>()

const { graph } = useCurrentProject()

const nodeRect = computed(() => graph.value?.nodeRects.get(props.nodeId))
const nodeHovered = computed(
  (): boolean => graph.value != null && graph.value.nodeHovered.get(props.nodeId),
)
const nodeExtended = computed(
  (): boolean => graph.value != null && graph.value.nodeExtended.get(props.nodeId),
)
const otherNodeHovered = computed(
  (): boolean => graph.value != null && !nodeHovered.value && graph.value.nodeHovered.exists.value,
)

const selection = useGraphSelection(true)
const { baseColor, selected, pending } = useComponentColors(
  () => graph.value?.db,
  selection,
  toRef(props, 'nodeId'),
)

// === Ports ===

interface PortData {
  clipRange: [number, number]
  label: string | undefined
  portId: AstId
}

const outputPortsSet = computed((): Set<AstId> => {
  if (!graph.value) return new Set()
  const bindings = graph.value.db.nodeOutputPorts.lookup(props.nodeId)
  if (bindings.size === 0) {
    const astId = graph.value.db.idFromExternal(props.nodeId)
    return new Set([astId].filter(isDef))
  }
  return bindings
})

const { componentBrowserOpened } = useGraphEditorState()

const outputPorts = computed((): PortData[] => {
  const ports = outputPortsSet.value
  const numPorts = ports.size
  return Array.from(ports, (portId, index): PortData => {
    return {
      clipRange: [index / numPorts, (index + 1) / numPorts],
      label: numPorts > 1 ? graph.value?.db.getOutputPortIdentifier(portId) : undefined,
      portId,
    }
  })
})

// === Interactivity ===

const mouseOverOutput = ref<AstId>()
// This is a counter rather than a boolean to ensure it remains accurate in the presence of multiple
// output ports, without relying on the order of events.
const mouseOverCreateNodeFromPortButton = ref(0)

const outputHovered = computed(() =>
  graph.value?.mouseEditedEdge ? undefined : mouseOverOutput.value,
)

function isPortDisconnected(portId: AstId): boolean {
  return graph.value != null && !graph.value.isConnectedSource(portId)
}

const anyPortDisconnected = computed(() =>
  setsIntersect(outputPortsSet.value, graph.value?.unconnectedEdgeSources),
)

const handlePortClick = useDoubleClick(
  (event: PointerEvent, portId: AstId) => emit('portClick', event, portId),
  (event: PointerEvent, portId: AstId) => emit('portDoubleClick', event, portId),
).handleClick

// === Rendering ===

const portsVisible = computed(
  () =>
    (nodeExtended.value && !otherNodeHovered.value) ||
    nodeHovered.value ||
    (outputHovered.value && outputPortsSet.value.has(outputHovered.value)) ||
    anyPortDisconnected.value ||
    mouseOverCreateNodeFromPortButton.value !== 0,
)

const portsHoverAnimation = useApproach(() => (portsVisible.value ? 1 : 0), 50, 0.01)

watchEffect(() => graph.value?.nodeOutputVisible.set(props.nodeId, portsVisible.value))
watchEffect(() => graph.value?.nodeOutputHovered.set(props.nodeId, outputHovered.value != null))
watchEffect(() => graph.value?.nodeOutputAnimations.set(props.nodeId, portsHoverAnimation.value))

const hoverAnimations = new Map<AstId, [ReturnType<typeof useApproach>, EffectScope]>()
watchEffect(() => {
  const ports = outputPortsSet.value
  for (const key of hoverAnimations.keys())
    if (!ports.has(key)) {
      hoverAnimations.get(key)?.[1].stop()
      hoverAnimations.delete(key)
    }
  for (const port of outputPortsSet.value) {
    setIfUndefined(hoverAnimations, port, () => {
      // Because `useApproach` uses `onScopeDispose` and we are calling it dynamically (i.e. not at
      // the setup top-level), we need to create a detached scope for each invocation.
      const scope = effectScope(true)
      const approach = scope.run(() =>
        useApproach(() => (outputHovered.value === port ? 1 : 0), 50, 0.01),
      )!
      return [approach, scope]
    })
  }
})

// Clean up dynamically created detached scopes.
onScopeDispose(() => hoverAnimations.forEach(([_, scope]) => scope.stop()))

const nodeStyle = computed(() => ({
  '--hover-animation': portsHoverAnimation.value,
  '--node-size-x': `${nodeRect.value?.size.x ?? 0}px`,
  '--node-size-y': `${nodeRect.value?.size.y ?? 0}px`,
  '--node-group-color': baseColor.value,
}))

const nodeTransform = computed(
  () => `translate(${nodeRect.value?.pos.x ?? 0}px, ${nodeRect.value?.pos.y ?? 0}px)`,
)

function portGroupStyle(port: PortData) {
  const [start, end] = port.clipRange
  return {
    '--direct-hover-animation': hoverAnimations.get(port.portId)?.[0].value ?? 0,
    '--port-clip-start': start,
    '--port-clip-end': end,
    '--port-label-transform-x': `${((end - start) / 2 + start) * 100}%`,
  }
}

function isPlusButtonVisible(portId: AstId): boolean {
  return !componentBrowserOpened.value && isPortDisconnected(portId)
}
function resetHoverState() {
  mouseOverOutput.value = undefined
  mouseOverCreateNodeFromPortButton.value = 0
}

// Opening component browser should manually clear output hover state, because
// plus button disappears when component browser opens, and we cannot receive pointerleave event
// in this case.
watch(componentBrowserOpened, (opened) => {
  if (opened) resetHoverState()
})

// When the plus button becomes invisible for the currently hovered port, we want to reset the hover state.
// This because we won’t receive a pointerleave event in this case.
watch(
  () => mouseOverOutput.value && isPlusButtonVisible(mouseOverOutput.value),
  (visible, prevVisible) => {
    if (prevVisible && !visible) resetHoverState()
  },
)

graph.value?.suggestEdgeFromOutput(outputHovered)
</script>

<template>
  <g
    class="GraphNodeOutputPorts define-node-colors"
    :style="nodeStyle"
    :class="{ selected, pending }"
    :data-output-ports-node-id="props.nodeId"
  >
    <g :style="{ transform: nodeTransform }">
      <template v-for="port of outputPorts" :key="port.portId">
        <g :style="portGroupStyle(port)">
          <g
            class="portClip"
            @pointerenter="mouseOverOutput = port.portId"
            @pointerleave="mouseOverOutput = undefined"
          >
            <g
              class="clickable"
              @pointerdown.stop="handlePortClick($event, port.portId)"
              @click.stop
            >
              <rect class="outputPortHoverArea" />
              <rect
                v-if="isPlusButtonVisible(port.portId)"
                class="createNodeButtonApproachZone"
              ></rect>
            </g>
            <rect class="outputPort" />
          </g>
          <text class="outputPortLabel">{{ port.label }}</text>
        </g>
      </template>
    </g>
    <template v-for="port of outputPorts" :key="port.portId">
      <CreateNodeFromPortButton
        v-if="isPlusButtonVisible(port.portId)"
        :portId="port.portId"
        :nodeId="nodeId"
        @update:hovered="mouseOverCreateNodeFromPortButton += $event ? 1 : -1"
        @newNodeClick="(port) => emit('newNodeClick', port)"
        @newNodeDrag="(port) => emit('newNodeDrag', port)"
      />
    </template>
  </g>
</template>

<style scoped>
.outputPort,
.outputPortHoverArea {
  x: calc(0px - var(--output-port-width) / 2);
  y: calc(0px - var(--output-port-width) / 2);
  height: calc(var(--node-size-y) + var(--output-port-width));
  width: calc(var(--node-size-x) + var(--output-port-width));
  rx: calc(var(--node-border-radius) + var(--output-port-width) / 2);

  fill: none;
  stroke: var(--color-edge-from-node);
  stroke-width: calc(var(--output-port-width) + var(--output-port-overlap-anim));
  transition: stroke 0.2s ease;
  --horizontal-line: calc(var(--node-size-x) - var(--node-border-radius) * 2);
  --vertical-line: calc(var(--node-size-y) - var(--node-border-radius) * 2);
  --radius-arclength: calc((var(--node-border-radius) + var(--output-port-width) * 0.5) * 2 * pi);

  stroke-dasharray: calc(var(--horizontal-line) + var(--radius-arclength) * 0.5) 10000%;
  stroke-dashoffset: calc(
    0px - var(--horizontal-line) - var(--vertical-line) - var(--radius-arclength) * 0.25
  );
  stroke-linecap: round;
}

.outputPort {
  --output-port-overlap-anim: calc(var(--hover-animation) * var(--output-port-overlap));
  --output-port-width: calc(
    var(--output-port-max-width) * var(--hover-animation) + var(--output-port-hovered-extra-width) *
      var(--direct-hover-animation) - var(--output-port-overlap-anim)
  );
  pointer-events: none;
}

.outputPortHoverArea {
  --output-port-width: var(--output-port-hover-width);
  stroke-width: var(--output-port-hover-width);
  stroke: transparent;
  /* Make stroke visible to debug the active area: */
  stroke-linecap: butt;
  pointer-events: stroke;
}

.GraphEditor.draggingEdge .outputPortHoverArea {
  display: none;
}

/* Feature-flag controlled debug display for hover areas. */
.App.debugHoverAreas .outputPortHoverArea {
  stroke: rgba(0, 0, 0, 0.1);
}

.portClip {
  clip-path: inset(
    0 calc((1 - var(--port-clip-end)) * (100% + 1px) - 0.5px) 0
      calc(var(--port-clip-start) * (100% + 1px) + 0.5px)
  );
}

.outputPortLabel {
  user-select: none;
  pointer-events: none;
  z-index: 10;
  text-anchor: middle;
  opacity: calc(var(--hover-animation) * var(--hover-animation));
  fill: var(--color-node-primary);
  transform: translate(
    var(--port-label-transform-x),
    calc(var(--node-size-y) + var(--output-port-max-width) + 16px)
  );
}

/**
 * Extension of the output port's hover area, to ensure the button doesn't disappear as the mouse is
 * moved toward it.
 */
.createNodeButtonApproachZone {
  --margin: 4px;
  --topOffset: 40px;
  --leftOffset: 16px;
  --radius: 10px;
  --width: calc(var(--radius) * 2 + var(--margin) * 2);
  pointer-events: fill;
  fill: transparent;
  width: var(--width);
  height: calc(
    var(--node-vertical-gap) + var(--output-port-max-width) + var(--margin) * 2 + var(--topOffset) +
      var(--radius)
  );
  transform: translate(
    calc(var(--port-clip-start) * (100% + 1px) + var(--leftOffset) - var(--width) / 2),
    calc(var(--node-size-y) + var(--output-port-max-width))
  );
  cursor: pointer;
}
</style>
