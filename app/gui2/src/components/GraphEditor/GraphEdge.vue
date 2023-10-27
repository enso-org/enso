<script setup lang="ts">
import { injectGraphNavigator } from '@/providers/graphNavigator.ts'
import { injectGraphSelection } from '@/providers/graphSelection.ts'
import type { Edge } from '@/stores/graph'
import type { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { clamp } from '@vueuse/core'
import type { ExprId } from 'shared/yjsModel'
import { computed, ref } from 'vue'

const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)

const props = defineProps<{
  edge: Edge
  nodeRects: Map<ExprId, Rect>
  exprRects: Map<ExprId, Rect>
  exprNodes: Map<ExprId, ExprId>
}>()

const emit = defineEmits<{
  disconnectSource: []
  disconnectTarget: []
}>()

const base = ref<SVGPathElement>()

type PosMaybeSized = { pos: Vec2; size?: Vec2 }

const targetPos = computed<PosMaybeSized | null>(() => {
  const targetExpr =
    props.edge.target ??
    (selection?.hoveredNode != props.edge.source ? selection?.hoveredExpr : undefined)
  if (targetExpr != null) {
    const targetNodeId = props.exprNodes.get(targetExpr)
    if (targetNodeId == null) return null
    const targetNodeRect = props.nodeRects.get(targetNodeId)
    const targetRect = props.exprRects.get(targetExpr)
    if (targetRect == null || targetNodeRect == null) return null
    return { pos: targetRect.center().add(targetNodeRect.pos), size: targetRect.size }
  } else if (navigator?.sceneMousePos != null) {
    return { pos: navigator?.sceneMousePos }
  } else {
    return null
  }
})
const sourcePos = computed<PosMaybeSized | null>(() => {
  const targetNode = props.edge.target != null ? props.exprNodes.get(props.edge.target) : undefined
  const sourceNode =
    props.edge.source ?? (selection?.hoveredNode != targetNode ? selection?.hoveredNode : undefined)
  if (sourceNode != null) {
    const sourceNodeRect = props.nodeRects.get(sourceNode)
    if (sourceNodeRect == null) return null
    const pos = sourceNodeRect.center()
    return { pos, size: sourceNodeRect.size }
  } else if (navigator?.sceneMousePos != null) {
    return { pos: navigator?.sceneMousePos }
  } else {
    return null
  }
})

/** The inputs to the edge state computation. */
type Inputs = {
  /** The width and height of the node that originates the edge, if any.
   *  The edge may begin anywhere around the bottom half of the node. */
  sourceSize: Vec2 | undefined
  /** The width and height of the port that the edge is attached to, if any. */
  targetSize: Vec2 | undefined
  /** The coordinates of the node input port that is the edge's destination, relative to the source position.
   *  The edge enters the port from above. */
  targetOffset: Vec2
}

type JunctionPoints = {
  points: Vec2[]
  maxRadius: number
  targetAttachment: { target: Vec2; length: number } | undefined
}

/** Minimum height above the target the edge must approach it from. */
const MIN_APPROACH_HEIGHT = 32.25
const NODE_HEIGHT = 32 // TODO (crate::component::node::HEIGHT)
const NODE_CORNER_RADIUS = 16 // TODO (crate::component::node::CORNER_RADIUS)
/** The preferred arc radius. */
const RADIUS_BASE = 20

/** Constants configuring the 1-corner layout. */
const SingleCorner = {
  /** The y-allocation for the radius will be the full available height minus this value. */
  RADIUS_Y_ADJUSTMENT: 29,
  /** The base x-allocation for the radius. */
  RADIUS_X_BASE: RADIUS_BASE,
  /** Proportion (0-1) of extra x-distance allocated to the radius. */
  RADIUS_X_FACTOR: 0.6,
  /** Distance for the line to continue under the node, to ensure that there isn't a gap. */
  SOURCE_NODE_OVERLAP: 4,
  /** Minimum arc radius at which we offset the source end to exit normal to the node's curve. */
  MINIMUM_TANGENT_EXIT_RADIUS: 2,
} as const

/** Constants configuring the 3-corner layouts. */
const ThreeCorner = {
  /** The maximum arc radius. */
  RADIUS_MAX: RADIUS_BASE,
  BACKWARD_EDGE_ARROW_THRESHOLD: 15,
  /** The maximum radius reduction (from [`RADIUS_BASE`]) to allow when choosing whether to use
   *  the three-corner layout that doesn't use a backward corner.
   */
  MAX_SQUEEZE: 2,
} as const

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
 *  given offset. Return the points, the maximum radius that should be used to draw the corners
 *  connecting them, and the length of the target attachment bit.
 */
function junctionPoints(inputs: Inputs): JunctionPoints | null {
  let halfSourceSize = inputs.sourceSize?.scale(0.5) ?? Vec2.Zero()
  // The maximum x-distance from the source (our local coordinate origin) for the point where the
  // edge will begin.
  const sourceMaxXOffset = Math.max(halfSourceSize.x - NODE_CORNER_RADIUS, 0)
  // The maximum y-length of the target-attachment segment. If the layout allows, the
  // target-attachment segment will fully exit the node before the first corner begins.
  const targetMaxAttachmentHeight =
    inputs.targetSize != null ? (NODE_HEIGHT - inputs.targetSize.y) / 2.0 : undefined
  const attachment =
    targetMaxAttachmentHeight != null
      ? {
          target: inputs.targetOffset.addScaled(new Vec2(0.0, NODE_HEIGHT), 0.5),
          length: targetMaxAttachmentHeight,
        }
      : undefined

  const targetWellBelowSource =
    inputs.targetOffset.y - (targetMaxAttachmentHeight ?? 0) >= MIN_APPROACH_HEIGHT
  const targetBelowSource = inputs.targetOffset.y > NODE_HEIGHT / 2.0
  const targetBeyondSource = Math.abs(inputs.targetOffset.x) > sourceMaxXOffset
  const horizontalRoomFor3Corners =
    targetBeyondSource &&
    Math.abs(inputs.targetOffset.x) - sourceMaxXOffset >=
      3.0 * (RADIUS_BASE - ThreeCorner.MAX_SQUEEZE)
  if (targetWellBelowSource || (targetBelowSource && !horizontalRoomFor3Corners)) {
    const {
      RADIUS_Y_ADJUSTMENT,
      RADIUS_X_BASE,
      RADIUS_X_FACTOR,
      SOURCE_NODE_OVERLAP,
      MINIMUM_TANGENT_EXIT_RADIUS,
    } = SingleCorner
    // The edge can originate anywhere along the length of the node.
    const sourceX = clamp(inputs.targetOffset.x, -sourceMaxXOffset, sourceMaxXOffset)
    const distanceX = Math.max(Math.abs(inputs.targetOffset.x) - halfSourceSize.x, 0)
    const radiusX = RADIUS_X_BASE + distanceX * RADIUS_X_FACTOR
    // The minimum length of straight line there should be at the target end of the edge. This
    // is a fixed value, except it is reduced when the target is horizontally very close to the
    // edge of the source, so that very short edges are less sharp.
    const yAdjustment = Math.min(
      Math.abs(inputs.targetOffset.x) - halfSourceSize.x + RADIUS_Y_ADJUSTMENT / 2.0,
      RADIUS_Y_ADJUSTMENT,
    )
    const radiusY = Math.max(Math.abs(inputs.targetOffset.y) - yAdjustment, 0.0)
    const maxRadius = Math.min(radiusX, radiusY)
    // The radius the edge would have, if the arc portion were as large as possible.
    const naturalRadius = Math.min(
      Math.abs(inputs.targetOffset.x - sourceX),
      Math.abs(inputs.targetOffset.y),
    )
    let sourceDY = 0
    if (naturalRadius > MINIMUM_TANGENT_EXIT_RADIUS) {
      // Offset the beginning of the edge so that it is normal to the curve of the source node
      // at the point that it exits the node.
      const radius = Math.min(naturalRadius, maxRadius)
      const arcOriginX = Math.abs(inputs.targetOffset.x) - radius
      const sourceArcOrigin = halfSourceSize.x - NODE_CORNER_RADIUS
      const circleOffset = arcOriginX - sourceArcOrigin
      const intersection = circleIntersection(circleOffset, NODE_CORNER_RADIUS, radius)
      sourceDY = -Math.abs(radius - intersection)
    } else if (halfSourceSize.y != 0) {
      sourceDY = -SOURCE_NODE_OVERLAP + halfSourceSize.y
    }
    const source = new Vec2(sourceX, sourceDY)
    // The target attachment will extend as far toward the edge of the node as it can without
    // rising above the source.
    let attachmentHeight =
      targetMaxAttachmentHeight != null
        ? Math.min(targetMaxAttachmentHeight, Math.abs(inputs.targetOffset.y))
        : 0
    let attachmentY = inputs.targetOffset.y - attachmentHeight - (inputs.targetSize?.y ?? 0) / 2.0
    let targetAttachment = new Vec2(inputs.targetOffset.x, attachmentY)
    return {
      points: [source, targetAttachment],
      maxRadius,
      targetAttachment: attachment,
    }
  } else {
    const { RADIUS_MAX } = ThreeCorner
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
      const j0Dx = Math.min(2 * RADIUS_MAX, distanceX / 2)
      const j1Dx = Math.min(RADIUS_MAX, (distanceX - j0Dx) / 2)
      j0x = sourceX + Math.abs(j0Dx) * signX
      j1x = j0x + Math.abs(j1Dx) * signX
      heightAdjustment = RADIUS_MAX - j1Dx
    } else {
      //            J1
      //           /
      //     ╭──────╮ J0
      //     ▢      │/
      // ╭─────╮    │
      // ╰─────╯────╯
      // J0 > source; J0 > J1; J1 > target.
      j1x = inputs.targetOffset.x + Math.abs(RADIUS_MAX) * signX
      const j0BeyondSource = Math.abs(inputs.targetOffset.x) + RADIUS_MAX * 2
      const j0BeyondTarget = Math.abs(sourceX) + RADIUS_MAX
      j0x = Math.abs(Math.max(j0BeyondTarget, j0BeyondSource)) * signX
      heightAdjustment = 0
    }
    if (j0x == null || j1x == null || heightAdjustment == null) return null
    const attachmentHeight = targetMaxAttachmentHeight ?? 0
    const top = Math.min(
      inputs.targetOffset.y - MIN_APPROACH_HEIGHT - attachmentHeight + heightAdjustment,
      0,
    )
    const source = new Vec2(sourceX, 0)
    const j0 = new Vec2(j0x, top / 2)
    const j1 = new Vec2(j1x, top)
    // The corners meet the target attachment at the top of the node.
    const attachmentTarget = attachment?.target ?? inputs.targetOffset
    return {
      points: [source, j0, j1, attachmentTarget],
      maxRadius: RADIUS_MAX,
      targetAttachment: attachment,
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
  if (start == null) return { start: Vec2.Zero(), elements: [] }
  let prev = start
  junctions.points.slice(1).map((j, i) => {
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
  const target_ = targetPos.value
  const source_ = sourcePos.value
  if (target_ == null || source_ == null) return null
  const inputs = {
    targetOffset: target_.pos.sub(source_.pos),
    sourceSize: source_.size,
    targetSize: target_.size,
  }
  return junctionPoints(inputs)
})

const basePath = computed(() => {
  if (props.edge.source == null && props.edge.target == null) return undefined
  const jp = currentJunctionPoints.value
  if (jp == null) return undefined
  const { start, elements } = pathElements(jp)
  const source_ = sourcePos.value
  if (source_ == null) return undefined
  return render(source_.pos.add(start), elements)
})

const activePath = computed(() => {
  if (hovered.value) return basePath.value
  else return undefined
})

function lengthTo(pos: Vec2): number | undefined {
  const path = base.value
  if (path == null) return undefined
  const totalLength = path.getTotalLength()
  let precision = 16
  let best: number | undefined
  let bestDist: number | undefined = undefined
  for (let i = 0; i < totalLength + precision; i += precision) {
    const len = Math.min(i, totalLength)
    const p = path.getPointAtLength(len)
    const dist = pos.distanceSquare(new Vec2(p.x, p.y))
    if (bestDist == null || dist < bestDist) {
      best = len
      bestDist = dist
    }
  }
  if (best == null || bestDist == null) return undefined
  const tryPos = (len: number) => {
    const point = path.getPointAtLength(len)
    const dist: number = pos.distanceSquare(new Vec2(point.x, point.y))
    if (bestDist == null || dist < bestDist) {
      best = len
      bestDist = dist
      return true
    }
    return false
  }
  for (; precision >= 0.5; precision /= 2) {
    tryPos(best + precision) || tryPos(best - precision)
  }
  return best
}

const hovered = ref(false)
const activeStyle = computed(() => {
  if (!hovered.value) return {}
  if (props.edge.source == null || props.edge.target == null) return {}
  if (base.value == null) return {}
  if (navigator?.sceneMousePos == null) return {}
  const length = base.value.getTotalLength()
  let offset = lengthTo(navigator?.sceneMousePos)
  if (offset == null) return {}
  offset = length - offset
  if (offset < length / 2) {
    offset += length
  }
  return {
    'stroke-dasharray': length,
    'stroke-dashoffset': offset,
  }
})

function click(_e: PointerEvent) {
  if (base.value == null) return {}
  if (navigator?.sceneMousePos == null) return {}
  const length = base.value.getTotalLength()
  let offset = lengthTo(navigator?.sceneMousePos)
  if (offset == null) return {}
  if (offset < length / 2) emit('disconnectTarget')
  else emit('disconnectSource')
}

function arrowPosition(): Vec2 | undefined {
  if (props.edge.source == null || props.edge.target == null) return
  const points = currentJunctionPoints.value?.points
  if (points == null || points.length < 3) return
  const target = targetPos.value
  const source = sourcePos.value
  if (target == null || source == null) return
  if (Math.abs(target.pos.y - source.pos.y) < ThreeCorner.BACKWARD_EDGE_ARROW_THRESHOLD) return
  if (points[1] == null) return
  return source.pos.add(points[1])
}

const arrowTransform = computed(() => {
  const pos = arrowPosition()
  if (pos != null) return `translate(${pos.x},${pos.y})`
  else return undefined
})
</script>

<template>
  <path
    v-if="basePath"
    :d="basePath"
    class="edge io"
    @pointerdown="click"
    @pointerenter="hovered = true"
    @pointerleave="hovered = false"
  />
  <path v-if="basePath" ref="base" :d="basePath" class="edge visible base" />
  <path v-if="activePath" :d="activePath" class="edge visible active" :style="activeStyle" />
  <polygon
    v-if="arrowTransform"
    :transform="arrowTransform"
    points="0,-9.375 -9.375,9.375 9.375,9.375"
    class="arrow visible"
  />
</template>

<style scoped>
.visible {
  pointer-events: none;
}
.arrow {
  fill: tan;
}
.edge {
  fill: none;
}
.edge.io {
  stroke-width: 14;
  stroke: transparent;
}
.edge.visible {
  stroke-width: 4;
}
.edge.visible.base {
  stroke: tan;
}
.edge.visible.active {
  stroke: red;
  stroke-linecap: round;
}
</style>
