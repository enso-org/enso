<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import type { NodeId } from '$/providers/openedProjects/graph'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref } from 'vue'
import type { AstId } from 'ydoc-shared/ast'

const hovered = defineModel<boolean>('hovered', { default: false })

const { portId, nodeId } = defineProps<{ portId: AstId; nodeId: NodeId }>()

const emit = defineEmits<{
  newNodeClick: [portId: AstId]
  newNodeDrag: [portId: AstId]
}>()

const { graph } = useCurrentProject()

const nodeRect = computed(() => graph.value?.nodeRects.get(nodeId))

const NODE_RADIUS = 16
const RADIUS = 10
const DISPLAY_OFFSET = new Vec2(NODE_RADIUS - RADIUS, NODE_RADIUS - RADIUS)

const OUTPUT_PORT_MAX_WIDTH = 4
const NODE_VERTICAL_GAP = 5
const progress = computed(() => graph.value?.nodeOutputAnimations.get(nodeId) ?? 0)

// Offset from the bottom left of the source node, when not being dragged or animated.
const RESTING_OFFSET = new Vec2(0, 40 + OUTPUT_PORT_MAX_WIDTH + NODE_VERTICAL_GAP)

const referencePoint = computed(
  () => new Vec2(nodeRect.value?.left ?? 0, nodeRect.value?.bottom ?? 0),
)
const restingPosition = computed(() => referencePoint.value.add(RESTING_OFFSET))
const position = computed(() =>
  referencePoint.value.add(Vec2.ElementwiseProduct(RESTING_OFFSET, new Vec2(1, progress.value))),
)

graph.value?.showCreateNodeButtonEdge(portId, {
  // Here we use the position at animation completion, rather than the currently-displayed position;
  // when displaying on hover, the edge length will be animated by the GraphEdge
  // `animateFromSourceHover`, which is more efficient since animates by length without recomputing
  // layout.
  position: computed(() => restingPosition.value.add(new Vec2(NODE_RADIUS, NODE_RADIUS - RADIUS))),
  hovered,
})

const clicked = ref(false)

function handlePointerDown() {
  clicked.value = true
}

function handlePointerUp() {
  clicked.value = false
  if (hovered.value) {
    emit('newNodeClick', portId)
  }
}

function handlePointerEnter() {
  hovered.value = true
}

function handlePointerLeave() {
  hovered.value = false
  if (clicked.value) {
    emit('newNodeDrag', portId)
  }
  clicked.value = false
}

function translate(offset: Vec2) {
  return `translate(${offset.x}px, ${offset.y}px)`
}
</script>

<template>
  <g
    class="CreateNodeFromPortButton clickable"
    :style="{
      transform: translate(position.add(DISPLAY_OFFSET)),
    }"
    :class="{ hovered }"
    @pointerdown.stop="handlePointerDown"
    @pointerup.stop="handlePointerUp"
    @pointerenter="handlePointerEnter"
    @pointerleave="handlePointerLeave"
  >
    <mask :id="`${portId}_add_node_clip_path`">
      <rect class="maskBackground"></rect>
      <rect class="plusV"></rect>
      <rect class="plusH"></rect>
    </mask>
    <circle
      :mask="`url(#${portId}_add_node_clip_path)`"
      fill="currentColor"
      class="plusButtonCircle"
    ></circle>
  </g>
</template>

<style scoped>
.CreateNodeFromPortButton {
  --radius: 10px;
  --maskSize: calc(var(--radius) * 2);
  --strokeWidth: 2px;
  position: absolute;
  top: 0;
  left: 0;
  pointer-events: all;
  color: color-mix(in oklab, var(--color-node-primary) 60%, white 40%);
  &.hovered {
    color: var(--color-node-primary);
  }
  transition: color 0.2s ease;
}

.maskBackground {
  fill: white;
  width: var(--maskSize);
  height: var(--maskSize);
}

.plusV {
  x: calc(var(--maskSize) / 2 - var(--strokeWidth) / 2);
  y: calc(var(--radius) / 2);
  width: var(--strokeWidth);
  height: var(--radius);
  fill: black;
}

.plusH {
  x: calc(var(--radius) / 2);
  y: calc(var(--maskSize) / 2 - var(--strokeWidth) / 2);
  width: var(--radius);
  height: var(--strokeWidth);
  fill: black;
}

.plusButtonCircle {
  cx: var(--radius);
  cy: var(--radius);
  /*noinspection CssUnresolvedCustomProperty*/
  r: calc(var(--radius) * var(--hover-animation));
}
</style>
