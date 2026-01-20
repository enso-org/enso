<script setup lang="ts">
import { useGraphStore } from '$/components/WithCurrentProject.vue'
import type { Edge } from '$/providers/openedProjects/graph'
import { isConnected } from '$/providers/openedProjects/graph'
import { connectedEdgeEquals } from '$/providers/openedProjects/graph/graph'
import { junctionPoints, pathElements, toSvgPath } from '@/components/GraphEditor/GraphEdge/layout'
import { useComponentColors } from '@/composables/componentColors'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { useGraphSelection } from '@/providers/graphSelection'
import { assert } from '@/util/assert'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import theme from '@/util/theme'
import { computed, type CSSProperties, ref, useAttrs } from 'vue'
import { EDGE_ARROW_MARKER_ID } from './GraphEdges.vue'

const selection = useGraphSelection(true)
const navigator = injectGraphNavigator(true)
const graph = useGraphStore()

const {
  edge,
  maskSource,
  animateFromSourceHover,
  arrow = true,
} = defineProps<{
  edge: Edge
  maskSource?: boolean
  animateFromSourceHover?: boolean
  arrow?: boolean
}>()
defineOptions({
  inheritAttrs: false,
})

// The padding added around the masking rect for nodes with visible output port. The actual padding
// is animated together with node's port opening. Required to correctly not draw the edge in space
// between the port path and node.
const VISIBLE_PORT_MASK_PADDING = 6
// A distance from edge's end target below which click will disconnect it instead of selecting.
const TARGET_DISCONNECT_THRESHOLD = 10

const base = ref<SVGPathElement>()
const hovered = ref(false)

const mouseAnchor = computed(() => 'anchor' in edge && edge.anchor.type === 'mouse')
const mouseAnchorPos = computed(() => (mouseAnchor.value ? navigator?.sceneMousePos : undefined))
const hoveredNode = computed(() => (mouseAnchor.value ? selection?.hoveredNode : undefined))
const hoveredPort = computed(() => (mouseAnchor.value ? selection?.hoveredPort : undefined))

const isSuggestion = computed(() => 'suggestion' in edge && edge.suggestion)

const connectedSourceNode = computed(() => edge.source && graph.getSourceNodeId(edge.source))

const sourceNode = computed(() => {
  if (connectedSourceNode.value) {
    return connectedSourceNode.value
  } else if (hoveredNode.value != null && edge.target) {
    // When the source is not set (i.e. edge is dragged), use the currently hovered over expression
    // as the source, as long as it is an output node or the same node as the target.
    const nodeType = graph.db.nodeIdToNode.get(hoveredNode.value)?.type
    const rawTargetNode = graph.getPortNodeId(edge.target)
    if (nodeType !== 'output' && hoveredNode.value != rawTargetNode) return hoveredNode.value
  }
  return undefined
})

const targetExpr = computed(() => {
  const setTarget = edge.target
  if (setTarget) {
    return setTarget
  } else if (hoveredNode.value != null && hoveredNode.value !== connectedSourceNode.value) {
    // When the target is not set (i.e. edge is dragged), use the currently hovered over expression
    // as the target, as long as it is not from the same node as the source.
    return hoveredPort.value
  }
  return undefined
})

const targetNode = computed(
  () => targetExpr.value && (graph.getPortNodeId(targetExpr.value) ?? hoveredNode.value),
)
const targetNodeRect = computed(() => targetNode.value && graph.nodeRects.get(targetNode.value))

/**
 * Offset between edge path end the and the target node rect. Needs to be big enough to leave space
 * for displaying the end marker (down arrow). Determined expermimentally to make it look good.
 */
const PATH_END_Y_OFFSET = -9

const targetPos = computed<Vec2 | undefined>(() => {
  const expr = targetExpr.value
  if (expr != null && targetNode.value != null && targetNodeRect.value != null) {
    const targetRectRelative = graph.getPortRelativeRect(expr)
    if (targetRectRelative == null) return
    return targetNodeRect.value.pos.add(new Vec2(targetRectRelative.center().x, PATH_END_Y_OFFSET))
  } else if (mouseAnchorPos.value != null) {
    return mouseAnchorPos.value
  } else if ('anchor' in edge && edge.anchor.type === 'fixed') {
    return edge.anchor.scenePos
  } else {
    return undefined
  }
})

const sourceNodeRect = computed<Rect | undefined>(() => {
  return sourceNode.value && graph.nodeRects.get(sourceNode.value)
})

const sourceRect = computed<Rect | undefined>(() => {
  if (sourceNodeRect.value) {
    return sourceNodeRect.value
  } else if (
    'anchor' in edge &&
    edge.anchor.type === 'mouse' &&
    edge.target != null &&
    mouseAnchorPos.value != null
  ) {
    return new Rect(mouseAnchorPos.value, Vec2.Zero)
  } else {
    return undefined
  }
})

/**
 * Edges which do not have `sourceRect` and `targetPos` initialized are marked by a special
 * `broken-edge` data-testid, for debugging and integration test purposes.
 */
const edgeIsBroken = computed(
  () =>
    sourceRect.value == null ||
    targetPos.value == null ||
    (sourceRect.value.pos.equals(targetPos.value) && sourceRect.value.size.equals(Vec2.Zero)),
)

type NodeMask = {
  rect: Rect
  radius: number
}

const startsInPort = computed(() => currentJunctionPoints.value?.startsInPort)
const sourceMask = computed<NodeMask | undefined>(() => {
  if (!maskSource && !startsInPort.value) return
  const nodeRect = sourceNodeRect.value
  if (!nodeRect) return
  const animProgress =
    startsInPort.value ?
      ((sourceNode.value && graph.nodeOutputAnimations.get(sourceNode.value)) ?? 0)
    : 0
  const padding = animProgress * VISIBLE_PORT_MASK_PADDING
  if (!maskSource && padding === 0) return
  const rect = nodeRect.expand(padding)
  const radius = 16 + padding
  return { rect, radius }
})

const {
  baseColor,
  selected: nodeSelected,
  pending,
} = useComponentColors(graph.db, selection, sourceNode)
const edgeSelected = computed(
  () =>
    selection?.selectedEdge != null &&
    isConnected(edge) &&
    connectedEdgeEquals(selection.selectedEdge, edge),
)
const selected = computed(() => nodeSelected.value || edgeSelected.value)

const sourceOriginPoint = computed(() => {
  const source = sourceRect.value
  if (source == null) return null
  const target = targetPos.value
  const targetAbove = target != null ? target.y < source.bottom : false
  const targetAside = target != null ? source.left > target.x || source.right < target.x : false
  const halfSourceHeight = (source.bottom - source.top) * 0.5
  const offset =
    targetAside || targetAbove ? Math.min(halfSourceHeight, theme.node.corner_radius) : 0
  const sourceStartPosY = Math.max(source.top + offset, source.bottom - offset)
  return new Vec2(source.center().x, sourceStartPosY)
})

const currentJunctionPoints = computed(() => {
  const target = targetPos.value
  const source = sourceRect.value
  const origin = sourceOriginPoint.value
  if (target == null || source == null || origin == null) return null

  return junctionPoints({
    sourceSize: source.size,
    targetOffset: target.sub(origin),
  })
})

const pathBoundingBox = computed(() => {
  const points = currentJunctionPoints.value?.points
  return points && Rect.FromPoints(...points)
})

const basePathElements = computed(() => {
  const jp = currentJunctionPoints.value
  if (jp == null) return undefined
  return pathElements(jp)
})

const basePath = computed(() => {
  const pathElements = basePathElements.value
  if (!pathElements) return
  const { start, elements } = pathElements
  const origin = sourceOriginPoint.value
  if (origin == null || !origin.isFinite() || !start.isFinite()) return undefined
  return toSvgPath(origin.add(start), elements)
})

const activePath = computed(
  () => hovered.value && clickWillDisconnect.value && edge.source != null && edge.target != null,
)

function lengthTo(path: SVGPathElement, pos: Vec2): number {
  const totalLength = path.getTotalLength()
  let best: number | undefined
  let bestDist: number | undefined
  const tryPos = (len: number) => {
    const dist = pos.distanceSquared(Vec2.FromXY(path.getPointAtLength(len)))
    if (bestDist == null || dist < bestDist) {
      best = len
      bestDist = dist
      return true
    }
    return false
  }

  tryPos(0)
  tryPos(totalLength)
  assert(best != null && bestDist != null)
  const precisionTarget = 0.5 / (navigator?.scale ?? 1)
  for (let precision = totalLength / 2; precision >= precisionTarget; precision /= 2) {
    if (!tryPos(best + precision)) tryPos(best - precision)
  }
  return best
}

const mouseLocationOnEdge = computed(() => {
  if (navigator?.sceneMousePos == null) return
  if (base.value == null) return
  const sourceToMouse = lengthTo(base.value, navigator.sceneMousePos)
  const sourceToTarget = base.value.getTotalLength()
  const mouseToTarget = sourceToTarget - sourceToMouse
  return { sourceToMouse, sourceToTarget, mouseToTarget }
})

const clickWillDisconnect = computed(
  () =>
    mouseLocationOnEdge.value != null &&
    mouseLocationOnEdge.value.mouseToTarget <= TARGET_DISCONNECT_THRESHOLD,
)

const activeStyle = computed(() => {
  if (!hovered.value) return {}
  if (edge.source == null || edge.target == null) return {}
  const distances = mouseLocationOnEdge.value
  if (distances == null) return {}
  return {
    strokeDasharray: distances.sourceToTarget,
    strokeDashoffset: distances.mouseToTarget,
  }
})

const baseStyle = computed(() => (baseColor.value ? { '--node-group-color': baseColor.value } : {}))

function click(event: PointerEvent) {
  if (!isConnected(edge)) return
  if (clickWillDisconnect.value) {
    graph.disconnectTarget(edge, event)
  } else if (selection) {
    selection.deselectAll()
    selection.selectedEdge = { ...edge }
  }
}

const VISIBILITY_HIDDEN = {
  visibility: 'hidden',
} as const

function truncateStrokeToLength(lengthPx: number) {
  return { strokeDasharray: `${lengthPx}px 1000000px` }
}

const sourceHoverAnimationStyle = computed((): CSSProperties => {
  if (!animateFromSourceHover) return {}
  if (!base.value || !sourceNode.value) return VISIBILITY_HIDDEN
  const progress = graph.nodeOutputAnimations.get(sourceNode.value)
  return (
    !progress ? VISIBILITY_HIDDEN
    : progress === 1 ? {}
    : truncateStrokeToLength(progress * base.value.getTotalLength())
  )
})

const baseClass = computed(() => {
  return { dimmed: activePath.value || isSuggestion.value, hovered: hovered.value }
})
const colorClasses = computed(() => {
  return { selected: selected.value, pending: pending.value }
})

const clipPath = computed(() => {
  const mask = sourceMask.value
  if (!mask) return
  const bounds = pathBoundingBox.value
  const origin = sourceOriginPoint.value
  if (!bounds || !origin) return

  // Expand bounding box enough to account for path width and all attached markers.
  const boundsPath = bounds.offsetBy(origin).expand(10).asSvgPath()
  const maskPath = mask.rect.asSvgPath(mask.radius)
  // Draw two paths on top of each other using `evenodd` clip rule, so in effect we
  // end up creating a `maskPath`-shaped hole within the path's bounds.
  return `path(evenodd, "${boundsPath} ${maskPath}") view-box`
})

const $attrs = useAttrs()
const groupAttrs = computed(() => {
  const clip = clipPath.value
  if (clip) {
    return { ...$attrs, 'clip-path': clip }
  } else {
    return $attrs
  }
})

const markerEnd = computed(() => (arrow ? `url(#${EDGE_ARROW_MARKER_ID})` : ''))
</script>

<template>
  <template v-if="basePath">
    <g
      v-bind="groupAttrs"
      class="GraphEdge"
      :data-source-node-id="sourceNode"
      :data-target-node-id="targetNode"
    >
      <path
        ref="base"
        :d="basePath"
        :marker-end="markerEnd"
        class="edge define-node-colors visible"
        :class="{ ...baseClass, ...colorClasses }"
        :style="{ ...baseStyle, ...sourceHoverAnimationStyle }"
      />
      <path
        v-if="isConnected(edge)"
        :d="basePath"
        class="edge io clickable"
        :data-testid="edgeIsBroken ? 'broken-edge' : null"
        @pointerdown.stop="click"
        @pointerenter="hovered = true"
        @pointerleave="hovered = false"
      />
      <path
        v-if="activePath"
        :d="basePath"
        class="edge define-node-colors visible"
        :class="colorClasses"
        :style="{ ...baseStyle, ...activeStyle }"
      />
    </g>
  </template>
</template>

<style scoped>
.visible {
  pointer-events: none;
  --node-group-color: var(--group-color-fallback);
}

.edge {
  fill: none;
  stroke: var(--color-edge-from-node);
  transition: stroke 0.2s ease;
  contain: strict;

  &.hovered {
    stroke: color-mix(in oklab, var(--color-edge-from-node), white 30%);
  }
}

.arrow {
  fill: var(--color-edge-from-node);
  transition: fill 0.2s ease;
}

.edge.io {
  stroke-width: 14;
  stroke: transparent;
  stroke-linecap: square;
  pointer-events: stroke;
}
.edge.visible {
  stroke-width: 4;
  stroke-linecap: round;
}

.edge.visible.dimmed {
  stroke: color-mix(in oklab, var(--color-edge-from-node) 60%, white 40%);
}

.arrow.visible.dimmed {
  fill: color-mix(in oklab, var(--color-edge-from-node) 60%, white 40%);
}
</style>
