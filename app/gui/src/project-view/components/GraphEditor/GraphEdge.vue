<script setup lang="ts">
import { junctionPoints, pathElements, toSvgPath } from '@/components/GraphEditor/GraphEdge/layout'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import type { Edge } from '@/stores/graph'
import { isConnected, useGraphStore } from '@/stores/graph'
import { assert } from '@/util/assert'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import theme from '@/util/theme'
import { computed, ref } from 'vue'

const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)
const graph = useGraphStore()

const { edge, maskSource, animateFromSourceHover } = defineProps<{
  edge: Edge
  maskSource?: boolean
  animateFromSourceHover?: boolean
}>()

// The padding added around the masking rect for nodes with visible output port. The actual padding
// is animated together with node's port opening. Required to correctly not draw the edge in space
// between the port path and node.
const VISIBLE_PORT_MASK_PADDING = 6

const base = ref<SVGPathElement>()

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

const targetPos = computed<Vec2 | undefined>(() => {
  const expr = targetExpr.value
  if (expr != null && targetNode.value != null && targetNodeRect.value != null) {
    const targetRectRelative = graph.getPortRelativeRect(expr)
    if (targetRectRelative == null) return
    const yAdjustment = -(arrowHeight + arrowYOffset)
    return targetNodeRect.value.pos.add(new Vec2(targetRectRelative.center().x, yAdjustment))
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
  id: string
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
      ((sourceNode.value && graph.nodeHoverAnimations.get(sourceNode.value)) ?? 0)
    : 0
  const padding = animProgress * VISIBLE_PORT_MASK_PADDING
  if (!maskSource && padding === 0) return
  const rect = nodeRect.expand(padding)
  const radius = 16 + padding
  const id = `mask_for_edge_to-${edge.target ?? 'unconnected'}`
  return { id, rect, radius }
})

const edgeColor = computed(() =>
  'color' in edge ? edge.color
  : sourceNode.value ? graph.db.getNodeColorStyle(sourceNode.value)
  : undefined,
)

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
  if (origin == null) return undefined
  return toSvgPath(origin.add(start), elements)
})

const activePath = computed(() => hovered.value && edge.source != null && edge.target != null)

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

const hovered = ref(false)
const activeStyle = computed(() => {
  if (!hovered.value) return {}
  if (edge.source == null || edge.target == null) return {}
  const distances = mouseLocationOnEdge.value
  if (distances == null) return {}
  const offset =
    distances.sourceToMouse < distances.mouseToTarget ?
      distances.mouseToTarget
    : -distances.sourceToMouse
  return {
    strokeDasharray: distances.sourceToTarget,
    strokeDashoffset: offset,
  }
})

const targetEndIsDimmed = computed(() => {
  if (isSuggestion.value) return true
  if (!hovered.value) return false
  const distances = mouseLocationOnEdge.value
  if (!distances) return false
  return distances.sourceToMouse < distances.mouseToTarget
})

const baseStyle = computed(() => ({ '--node-base-color': edgeColor.value ?? 'tan' }))

function click(event: PointerEvent) {
  const distances = mouseLocationOnEdge.value
  if (distances == null) return
  if (!isConnected(edge)) return
  if (distances.sourceToMouse < distances.mouseToTarget) graph.disconnectTarget(edge, event)
  else graph.disconnectSource(edge, event)
}

function svgTranslate(offset: Vec2): string {
  return `translate(${offset.x},${offset.y})`
}

const backwardEdgeArrowTransform = computed<string | undefined>(() => {
  if (edge.source == null || edge.target == null) return
  const points = currentJunctionPoints.value?.points
  if (points == null || points.length < 3) return
  const target = targetPos.value
  const origin = sourceOriginPoint.value
  if (target == null || origin == null) return
  if (target.y > origin.y - theme.edge.three_corner.backward_edge_arrow_threshold) return
  if (points[1] == null) return
  return svgTranslate(origin.add(points[1]))
})

const arrowHeight = 9
const arrowYOffset = 0
const arrowTransform = computed<string | undefined>(() => {
  const arrowTopOffset = 1
  const arrowWidth = 12
  const target = targetPos.value
  if (target == null) return
  const pos = target.sub(new Vec2(arrowWidth / 2, arrowTopOffset))
  return svgTranslate(pos)
})

const arrowPath = [
  'M10.9635 1.5547',
  'L6.83205 7.75193',
  'C6.43623 8.34566 5.56377 8.34566 5.16795 7.75192',
  'L1.03647 1.5547',
  'C0.593431 0.890146 1.06982 0 1.86852 0',
  'L10.1315 0',
  'C10.9302 0 11.4066 0.890147 10.9635 1.5547',
  'Z',
].join('')

const sourceHoverAnimationStyle = computed(() => {
  if (!animateFromSourceHover || !base.value || !sourceNode.value) return {}
  const progress = graph.nodeHoverAnimations.get(sourceNode.value) ?? 0
  if (progress === 1) return {}
  const currentLength = progress * base.value.getTotalLength()
  return {
    strokeDasharray: `${currentLength}px 1000000px`,
  }
})

const baseClass = computed(() => {
  return { dimmed: activePath.value || isSuggestion.value }
})
</script>

<template>
  <template v-if="basePath">
    <mask
      v-if="sourceMask && navigator"
      :id="sourceMask.id"
      :x="navigator.viewport.left"
      :y="navigator.viewport.top"
      width="100%"
      height="100%"
      maskUnits="userSpaceOnUse"
    >
      <rect
        :x="navigator.viewport.left"
        :y="navigator.viewport.top"
        width="100%"
        height="100%"
        fill="white"
      />
      <rect
        :x="sourceMask.rect.left"
        :y="sourceMask.rect.top"
        :width="sourceMask.rect.width"
        :height="sourceMask.rect.height"
        :rx="sourceMask.radius"
        :ry="sourceMask.radius"
        fill="black"
      />
    </mask>
    <g v-bind="sourceMask && { mask: `url('#${sourceMask.id}')` }">
      <path
        ref="base"
        :d="basePath"
        class="edge visible"
        :class="baseClass"
        :style="{ ...baseStyle, ...sourceHoverAnimationStyle }"
        :data-source-node-id="sourceNode"
        :data-target-node-id="targetNode"
      />
      <path
        v-if="isConnected(edge)"
        :d="basePath"
        class="edge io clickable"
        :data-source-node-id="sourceNode"
        :data-target-node-id="targetNode"
        :data-testid="edgeIsBroken ? 'broken-edge' : null"
        @pointerdown.stop="click"
        @pointerenter="hovered = true"
        @pointerleave="hovered = false"
      />
      <path
        v-if="activePath"
        :d="basePath"
        class="edge visible"
        :style="{ ...baseStyle, ...activeStyle }"
        :data-source-node-id="sourceNode"
        :data-target-node-id="targetNode"
      />
      <path
        v-if="arrowTransform"
        :transform="arrowTransform"
        :d="arrowPath"
        class="arrow visible"
        :class="{ dimmed: targetEndIsDimmed }"
        :style="baseStyle"
      />
      <polygon
        v-if="backwardEdgeArrowTransform"
        :transform="backwardEdgeArrowTransform"
        points="0,-9.375 -9.375,9.375 9.375,9.375"
        class="arrow visible"
        :style="baseStyle"
        :data-source-node-id="sourceNode"
        :data-target-node-id="targetNode"
      />
    </g>
  </template>
</template>

<style scoped>
.visible {
  pointer-events: none;
  --edge-color: color-mix(in oklab, var(--node-base-color) 85%, white 15%);
}

.edge {
  fill: none;
  stroke: var(--edge-color);
  transition: stroke 0.2s ease;
  contain: strict;
}

.arrow {
  fill: var(--edge-color);
  transition: fill 0.2s ease;
}

.edge.io {
  stroke-width: 14;
  stroke: transparent;
  pointer-events: stroke;
}
.edge.visible {
  stroke-width: 4;
  stroke-linecap: round;
}

.edge.visible.dimmed {
  /* stroke: rgba(255, 255, 255, 0.4); */
  stroke: color-mix(in oklab, var(--edge-color) 60%, white 40%);
}

.arrow.visible.dimmed {
  fill: color-mix(in oklab, var(--edge-color) 60%, white 40%);
}
</style>
