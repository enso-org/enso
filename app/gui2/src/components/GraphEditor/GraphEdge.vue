<script setup lang="ts">
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { isConnected, useGraphStore, type Edge } from '@/stores/graph'
import { assert } from '@/util/assert'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import theme from '@/util/theme'
import { clamp } from '@vueuse/core'
import { computed, ref } from 'vue'

const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)
const graph = useGraphStore()

const props = defineProps<{
  edge: Edge
  maskSource?: boolean
}>()

const base = ref<SVGPathElement>()

const sourceNode = computed(() => {
  const setSource = props.edge.source
  if (setSource != null) {
    return graph.db.getPatternExpressionNodeId(setSource)
  } else {
    // When the source is not set (i.e. edge is dragged), use the currently hovered over expression
    // as the source, as long as it is not from the same node as the target.
    if (selection?.hoveredNode != null) {
      const rawTargetNode = props.edge.target && graph.getPortNodeId(props.edge.target)
      if (selection.hoveredNode != rawTargetNode) return selection.hoveredNode
    }
  }
  return undefined
})

const targetExpr = computed(() => {
  const setTarget = props.edge.target
  // When the target is not set (i.e. edge is dragged), use the currently hovered over expression
  // as the target, as long as it is not from the same node as the source.
  if (setTarget == null && selection?.hoveredNode != null) {
    if (selection.hoveredNode != props.edge.source) return selection.hoveredPort
  }
  return setTarget
})

const targetNode = computed(
  () => targetExpr.value && (graph.getPortNodeId(targetExpr.value) ?? selection?.hoveredNode),
)
const targetNodeRect = computed(() => targetNode.value && graph.nodeRects.get(targetNode.value))

const targetPos = computed<Vec2 | undefined>(() => {
  const expr = targetExpr.value
  if (expr != null && targetNode.value != null && targetNodeRect.value != null) {
    const targetRectRelative = graph.getPortRelativeRect(expr)
    if (targetRectRelative == null) return
    const yAdjustment =
      targetIsSelfArgument.value ? -(selfArgumentArrowHeight + selfArgumentArrowYOffset) : 0
    return targetNodeRect.value.pos.add(new Vec2(targetRectRelative.center().x, yAdjustment))
  } else if (navigator?.sceneMousePos != null) {
    return navigator.sceneMousePos
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
  } else if (navigator?.sceneMousePos != null) {
    return new Rect(navigator.sceneMousePos, Vec2.Zero)
  } else {
    return undefined
  }
})

/** Edges which do not have `sourceRect` and `targetPos` initialized are marked by a special
 * `broken-edge` data-testid, for debugging and e2e test purposes. */
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
const sourceMask = computed<NodeMask | undefined>(() => {
  if (!props.maskSource) return
  const rect = sourceNodeRect.value
  if (!rect) return
  const radius = 16
  const id = `mask_for_edge_to-${props.edge.target ?? 'unconnected'}`
  return { id, rect, radius }
})

const edgeColor = computed(
  () =>
    (targetNode.value && graph.db.getNodeColorStyle(targetNode.value)) ??
    (sourceNode.value && graph.db.getNodeColorStyle(sourceNode.value)),
)

/** The inputs to the edge state computation. */
interface Inputs {
  /** The width and height of the node that originates the edge, if any.
   *  The edge may begin anywhere around the bottom half of the node. */
  sourceSize: Vec2
  /** The coordinates of the node input port that is the edge's destination, relative to the source
   * position. The edge enters the port from above. */
  targetOffset: Vec2
}

interface JunctionPoints {
  points: Vec2[]
  maxRadius: number
}

function circleIntersection(x: number, r1: number, r2: number): number {
  let xNorm = clamp(x, -r2, r1)
  return Math.sqrt(r1 * r1 + r2 * r2 - xNorm * xNorm)
}

/** Edge layout calculation.
 *
 *  # Corners
 *
 *  ```text
 *    ────╮
 *  ```
 *
 *  The fundamental unit of edge layout is the [`Corner`]. A corner is a line segment attached to a
 *  90° arc. The length of the straight segment, the radius of the arc, and the orientation of the
 *  shape may vary. Any shape of edge is built from corners.
 *
 *  The shape of a corner can be fully-specified by two points: The horizontal end, and the vertical
 *  end.
 *
 *  In special cases, a corner may be *trivial*: It may have a radius of zero, in which case either
 *  the horizontal or vertical end will not be in the usual orientation. The layout algorithm only
 *  produces trivial corners when the source is directly in line with the target, or in some cases
 *  when subdividing a corner (see [Partial edges] below).
 *
 *  # Junction points
 *
 *  ```text
 *               3
 *    1         /
 *     \    ╭─────╮
 *      ────╯\     \
 *            2     4
 *  ```
 *
 *  The layout algorithm doesn't directly place corners. The layout algorithm places a sequence of
 *  junction points--coordinates where two horizontal corner ends or two vertical corner ends meet
 *  (or just one corner end, at an end of an edge). A series of junction points, always alternating
 *  horizontal/vertical, has a one-to-one relationship with a sequence of corners.
 */

/** Calculate the start and end positions of each 1-corner section composing an edge to the
 *  given offset. Return the points and the maximum radius that should be used to draw the corners
 *  connecting them.
 */
function junctionPoints(inputs: Inputs): JunctionPoints | null {
  let halfSourceSize = inputs.sourceSize?.scale(0.5) ?? Vec2.Zero
  // The maximum x-distance from the source (our local coordinate origin) for the point where the
  // edge will begin.
  const sourceMaxXOffset = Math.max(halfSourceSize.x - theme.node.corner_radius, 0)
  const attachmentTarget = inputs.targetOffset
  const targetWellBelowSource = inputs.targetOffset.y >= theme.edge.min_approach_height
  const targetBelowSource = inputs.targetOffset.y > 0
  const targetBeyondSource = Math.abs(inputs.targetOffset.x) > sourceMaxXOffset
  const horizontalRoomFor3Corners =
    targetBeyondSource &&
    Math.abs(inputs.targetOffset.x) - sourceMaxXOffset >=
      3.0 * (theme.edge.radius - theme.edge.three_corner.max_squeeze)
  if (targetWellBelowSource || (targetBelowSource && !horizontalRoomFor3Corners)) {
    const innerTheme = theme.edge.one_corner
    // The edge can originate anywhere along the length of the node.
    const sourceX = clamp(inputs.targetOffset.x, -sourceMaxXOffset, sourceMaxXOffset)
    const distanceX = Math.max(Math.abs(inputs.targetOffset.x) - halfSourceSize.x, 0)
    const radiusX = innerTheme.radius_x_base + distanceX * innerTheme.radius_x_factor
    // The minimum length of straight line there should be at the target end of the edge. This
    // is a fixed value, except it is reduced when the target is horizontally very close to the
    // edge of the source, so that very short edges are less sharp.
    const yAdjustment = Math.min(
      Math.abs(inputs.targetOffset.x) - halfSourceSize.x + innerTheme.radius_y_adjustment / 2.0,
      innerTheme.radius_y_adjustment,
    )
    const radiusY = Math.max(Math.abs(inputs.targetOffset.y) - yAdjustment, 0.0)
    const maxRadius = Math.min(radiusX, radiusY)
    // The radius the edge would have, if the arc portion were as large as possible.
    const naturalRadius = Math.min(
      Math.abs(inputs.targetOffset.x - sourceX),
      Math.abs(inputs.targetOffset.y),
    )
    let sourceDY = 0
    if (naturalRadius > innerTheme.minimum_tangent_exit_radius) {
      // Offset the beginning of the edge so that it is normal to the curve of the source node
      // at the point that it exits the node.
      const radius = Math.min(naturalRadius, maxRadius)
      const arcOriginX = Math.abs(inputs.targetOffset.x) - radius
      const sourceArcOrigin = halfSourceSize.x - theme.node.corner_radius
      const circleOffset = arcOriginX - sourceArcOrigin
      const intersection = circleIntersection(circleOffset, theme.node.corner_radius, radius)
      sourceDY = -Math.abs(radius - intersection)
    } else if (halfSourceSize.y != 0) {
      sourceDY = -innerTheme.source_node_overlap + halfSourceSize.y
    }
    const source = new Vec2(sourceX, sourceDY)
    return {
      points: [source, inputs.targetOffset],
      maxRadius,
    }
  } else {
    const radiusMax = theme.edge.three_corner.radius_max
    // The edge originates from either side of the node.
    const signX = Math.sign(inputs.targetOffset.x)
    const sourceX = Math.abs(sourceMaxXOffset) * signX
    const distanceX = Math.abs(inputs.targetOffset.x - sourceX)
    let j0x: number
    let j1x: number
    let heightAdjustment: number
    if (horizontalRoomFor3Corners) {
      //               J1
      //              /
      //            ╭──────╮
      // ╭─────╮    │      ▢
      // ╰─────╯────╯\
      //             J0
      // Junctions (J0, J1) are in between source and target.
      const j0Dx = Math.min(2 * radiusMax, distanceX / 2)
      const j1Dx = Math.min(radiusMax, (distanceX - j0Dx) / 2)
      j0x = sourceX + Math.abs(j0Dx) * signX
      j1x = j0x + Math.abs(j1Dx) * signX
      heightAdjustment = radiusMax - j1Dx
    } else {
      //            J1
      //           /
      //     ╭──────╮ J0
      //     ▢      │/
      // ╭─────╮    │
      // ╰─────╯────╯
      // J0 > source; J0 > J1; J1 > target.
      j1x = inputs.targetOffset.x + Math.abs(radiusMax) * signX
      const j0BeyondSource = Math.abs(inputs.targetOffset.x) + radiusMax * 2
      const j0BeyondTarget = Math.abs(sourceX) + radiusMax
      j0x = Math.abs(Math.max(j0BeyondTarget, j0BeyondSource)) * signX
      heightAdjustment = 0
    }
    if (j0x == null || j1x == null || heightAdjustment == null) return null
    const top = Math.min(
      inputs.targetOffset.y - theme.edge.min_approach_height + heightAdjustment,
      0,
    )
    const source = new Vec2(sourceX, 0)
    const j0 = new Vec2(j0x, top / 2)
    const j1 = new Vec2(j1x, top)
    return {
      points: [source, j0, j1, attachmentTarget],
      maxRadius: radiusMax,
    }
  }
}

type Line = { axis: 'h' | 'v'; length: number }
type Arc = { radius: number; signX: number; signY: number; sweep: 0 | 1 }
type Element = Arc | Line

function pathElements(junctions: JunctionPoints): { start: Vec2; elements: Element[] } {
  const elements: Element[] = []
  const pushLine = (line: Line) => {
    if (line.length === 0) return
    const e = elements.pop()
    if (e != null) {
      if ('axis' in e && e.axis == line.axis) {
        e.length += line.length
        elements.push(e)
      } else {
        elements.push(e)
        elements.push(line)
      }
    } else {
      elements.push(line)
    }
  }
  const start = junctions.points[0]
  if (start == null) return { start: Vec2.Zero, elements: [] }
  let prev = start
  junctions.points.slice(1).forEach((j, i) => {
    const d = j.sub(prev)
    const radius = Math.min(junctions.maxRadius, Math.abs(d.x), Math.abs(d.y))
    const signX = Math.sign(d.x)
    const signY = Math.sign(d.y)
    const dx = (Math.abs(d.x) - radius) * signX
    const dy = (Math.abs(d.y) - radius) * signY
    const h: Line = { axis: 'h', length: dx }
    const v: Line = { axis: 'v', length: dy }
    const sweep = (signX === signY) === (i % 2 === 0) ? 1 : 0
    if (i % 2 == 0) {
      pushLine(h)
      elements.push({ radius, signX, signY, sweep })
      pushLine(v)
    } else {
      pushLine(v)
      elements.push({ radius, signX, signY, sweep })
      pushLine(h)
    }
    prev = j
  })
  return { start, elements }
}

function render(sourcePos: Vec2, elements: Element[]): string {
  let out = `M ${sourcePos.x} ${sourcePos.y}`
  for (const e of elements) {
    if ('axis' in e) {
      out += ` ${e.axis} ${e.length}`
    } else {
      const dx = e.radius * e.signX
      const dy = e.radius * e.signY
      out += ` a ${e.radius} ${e.radius} 0 0 ${e.sweep} ${dx} ${dy}`
    }
  }
  return out
}

const currentJunctionPoints = computed(() => {
  const target = targetPos.value
  const source = sourceRect.value
  if (target == null || source == null) return null
  const inputs: Inputs = {
    sourceSize: source.size,
    targetOffset: target.sub(source.center()),
  }
  return junctionPoints(inputs)
})

const basePath = computed(() => {
  if (props.edge.source == null && props.edge.target == null) return undefined
  const jp = currentJunctionPoints.value
  if (jp == null) return undefined
  const { start, elements } = pathElements(jp)
  const source_ = sourceRect.value
  if (source_ == null) return undefined
  return render(source_.center().add(start), elements)
})

const activePath = computed(() => {
  if (hovered.value && props.edge.source != null && props.edge.target != null) return basePath.value
  else return undefined
})

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

  tryPos(0), tryPos(totalLength)
  assert(best != null && bestDist != null)
  const precisionTarget = 0.5 / (navigator?.scale ?? 1)
  for (let precision = totalLength / 2; precision >= precisionTarget; precision /= 2) {
    tryPos(best + precision) || tryPos(best - precision)
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
  if (props.edge.source == null || props.edge.target == null) return {}
  const distances = mouseLocationOnEdge.value
  if (distances == null) return {}
  const offset =
    distances.sourceToMouse < distances.mouseToTarget ?
      distances.mouseToTarget
    : -distances.sourceToMouse
  return {
    ...baseStyle.value,
    strokeDasharray: distances.sourceToTarget,
    strokeDashoffset: offset,
  }
})

const targetEndIsDimmed = computed(() => {
  if (!hovered.value) return false
  const distances = mouseLocationOnEdge.value
  if (!distances) return false
  return distances.sourceToMouse < distances.mouseToTarget
})

const baseStyle = computed(() => ({ '--node-base-color': edgeColor.value ?? 'tan' }))

function click() {
  const distances = mouseLocationOnEdge.value
  if (distances == null) return
  if (distances.sourceToMouse < distances.mouseToTarget) graph.disconnectTarget(props.edge)
  else graph.disconnectSource(props.edge)
}

function svgTranslate(offset: Vec2): string {
  return `translate(${offset.x},${offset.y})`
}

const backwardEdgeArrowTransform = computed<string | undefined>(() => {
  if (props.edge.source == null || props.edge.target == null) return
  const points = currentJunctionPoints.value?.points
  if (points == null || points.length < 3) return
  const target = targetPos.value
  const source = sourceRect.value
  if (target == null || source == null) return
  if (target.y > source.pos.y - theme.edge.three_corner.backward_edge_arrow_threshold) return
  if (points[1] == null) return
  return svgTranslate(source.center().add(points[1]))
})

const targetIsSelfArgument = computed(() => {
  if (!targetExpr.value) return
  const nodeId = graph.getPortNodeId(targetExpr.value)
  if (!nodeId) return
  const primarySubject = graph.db.nodeIdToNode.get(nodeId)?.primarySubject
  if (!primarySubject) return
  return targetExpr.value === primarySubject
})

const selfArgumentArrowHeight = 9
const selfArgumentArrowYOffset = 0
const selfArgumentArrowTransform = computed<string | undefined>(() => {
  const selfArgumentArrowTopOffset = 4
  const selfArgumentArrowWidth = 12
  if (!targetIsSelfArgument.value) return
  const target = targetPos.value
  if (target == null) return
  const pos = target.sub(new Vec2(selfArgumentArrowWidth / 2, selfArgumentArrowTopOffset))
  return svgTranslate(pos)
})

const selfArgumentArrowPath = [
  'M10.9635 1.5547',
  'L6.83205 7.75193',
  'C6.43623 8.34566 5.56377 8.34566 5.16795 7.75192',
  'L1.03647 1.5547',
  'C0.593431 0.890146 1.06982 0 1.86852 0',
  'L10.1315 0',
  'C10.9302 0 11.4066 0.890147 10.9635 1.5547',
  'Z',
].join('')

const connected = computed(() => isConnected(props.edge))
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
        v-if="activePath"
        :d="basePath"
        class="edge visible dimmed"
        :style="baseStyle"
        :data-source-node-id="sourceNode"
        :data-target-node-id="targetNode"
      />
      <path
        v-if="connected"
        :d="basePath"
        class="edge io"
        :data-source-node-id="sourceNode"
        :data-target-node-id="targetNode"
        :data-testid="edgeIsBroken ? 'broken-edge' : null"
        @pointerdown.stop="click"
        @pointerenter="hovered = true"
        @pointerleave="hovered = false"
      />
      <path
        ref="base"
        :d="activePath ?? basePath"
        class="edge visible"
        :style="activePath ? activeStyle : baseStyle"
        :data-source-node-id="sourceNode"
        :data-target-node-id="targetNode"
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
      <path
        v-if="selfArgumentArrowTransform"
        :transform="selfArgumentArrowTransform"
        :d="selfArgumentArrowPath"
        :class="{ arrow: true, visible: true, dimmed: targetEndIsDimmed }"
        :style="baseStyle"
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
