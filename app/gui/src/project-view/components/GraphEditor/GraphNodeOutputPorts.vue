<script setup lang="ts">
import { useApproach } from '@/composables/animation'
import { useDoubleClick } from '@/composables/doubleClick'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { isDef } from '@vueuse/core'
import { setIfUndefined } from 'lib0/map'
import {
  computed,
  effectScope,
  onScopeDispose,
  ref,
  watch,
  watchEffect,
  type EffectScope,
} from 'vue'
import type { AstId } from 'ydoc-shared/ast'

const props = defineProps<{ nodeId: NodeId; forceVisible: boolean }>()

const emit = defineEmits<{
  portClick: [event: PointerEvent, portId: AstId]
  portDoubleClick: [event: PointerEvent, portId: AstId]
  'update:hoverAnim': [progress: number]
  'update:nodeHovered': [hovered: boolean]
}>()

const graph = useGraphStore()

// === Ports ===

interface PortData {
  clipRange: [number, number]
  label: string | undefined
  portId: AstId
}

const outputPortsSet = computed(() => {
  const bindings = graph.db.nodeOutputPorts.lookup(props.nodeId)
  if (bindings.size === 0) {
    const astId = graph.db.idFromExternal(props.nodeId)
    return new Set([astId].filter(isDef))
  }
  return bindings
})

const outputPorts = computed((): PortData[] => {
  const ports = outputPortsSet.value
  const numPorts = ports.size
  return Array.from(ports, (portId, index): PortData => {
    return {
      clipRange: [index / numPorts, (index + 1) / numPorts],
      label: numPorts > 1 ? graph.db.getOutputPortIdentifier(portId) : undefined,
      portId,
    }
  })
})

// === Interactivity ===

const mouseOverOutput = ref<AstId>()

const outputHovered = computed(() => (graph.mouseEditedEdge ? undefined : mouseOverOutput.value))
watch(outputHovered, (newVal, oldVal) => {
  if ((newVal != null) !== (oldVal != null)) {
    emit('update:nodeHovered', newVal != null)
  }
})

const anyPortDisconnected = computed(() => {
  for (const port of outputPortsSet.value) {
    if (graph.unconnectedEdgeSources.has(port)) return true
  }
  return false
})

const handlePortClick = useDoubleClick(
  (event: PointerEvent, portId: AstId) => emit('portClick', event, portId),
  (event: PointerEvent, portId: AstId) => emit('portDoubleClick', event, portId),
).handleClick

// === Rendering ===

const portsVisible = computed(
  () =>
    props.forceVisible ||
    (outputHovered.value && outputPortsSet.value.has(outputHovered.value)) ||
    anyPortDisconnected.value,
)

const portsHoverAnimation = useApproach(() => (portsVisible.value ? 1 : 0), 50, 0.01)

watchEffect(() => emit('update:hoverAnim', portsHoverAnimation.value))

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

function portGroupStyle(port: PortData) {
  const [start, end] = port.clipRange
  return {
    '--hover-animation': portsHoverAnimation.value,
    '--direct-hover-animation': hoverAnimations.get(port.portId)?.[0].value ?? 0,
    '--port-clip-start': start,
    '--port-clip-end': end,
    transform: 'var(--output-port-transform)',
  }
}

graph.suggestEdgeFromOutput(outputHovered)
</script>

<template>
  <template v-for="port of outputPorts" :key="port.portId">
    <g :style="portGroupStyle(port)">
      <g class="portClip">
        <rect
          class="outputPortHoverArea clickable"
          @pointerenter="mouseOverOutput = port.portId"
          @pointerleave="mouseOverOutput = undefined"
          @pointerdown.stop.prevent="handlePortClick($event, port.portId)"
        />
        <rect class="outputPort" />
      </g>
      <text class="outputPortLabel">{{ port.label }}</text>
    </g>
  </template>
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
  stroke: var(--node-color-port);
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
  /* stroke: red; */
  stroke-linecap: butt;
  pointer-events: stroke;
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
  fill: var(--node-color-primary);
  transform: translate(50%, calc(var(--node-size-y) + var(--output-port-max-width) + 16px));
}
</style>
