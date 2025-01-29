import { Vec2 } from '@/util/data/vec2'
import theme from '@/util/theme'
import { clamp } from '@vueuse/core'

/** The inputs to the edge state computation. */
export interface Inputs {
  /**
   * The width and height of the node that originates the edge, if any.
   *  The edge may begin anywhere around the bottom half of the node.
   */
  sourceSize: Vec2
  /**
   * The coordinates of the node input port that is the edge's destination, relative to the source
   * position. The edge enters the port from above.
   */
  targetOffset: Vec2
}

export interface JunctionPoints {
  points: Vec2[]
  maxRadius: number
  startsInPort: boolean
}

/**
 * Edge layout calculation.
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

/**
 * Calculate the start and end positions of each 1-corner section composing an edge to the
 *  given offset. Return the points and the maximum radius that should be used to draw the corners
 *  connecting them.
 */
export function junctionPoints(inputs: Inputs): JunctionPoints | null {
  const halfSourceSize = inputs.sourceSize?.scale(0.5) ?? Vec2.Zero
  // The maximum x-distance from the source (our local coordinate origin) for the point where the
  // edge will begin.
  const sourceMaxXOffset = Math.max(halfSourceSize.x, 0)
  const attachmentTarget = inputs.targetOffset
  const targetWellBelowSource = inputs.targetOffset.y >= theme.edge.min_approach_height
  const targetBelowSource = inputs.targetOffset.y > 0
  const targetBeyondSource = Math.abs(inputs.targetOffset.x) > sourceMaxXOffset
  const horizontalRoomFor3Corners =
    targetBeyondSource &&
    Math.abs(inputs.targetOffset.x) - sourceMaxXOffset >=
      3.0 * (theme.edge.radius - theme.edge.three_corner.max_squeeze)
  const horizontalRoomFor3CornersNoSqueeze =
    targetBeyondSource &&
    Math.abs(inputs.targetOffset.x) - sourceMaxXOffset >=
      3.0 * theme.edge.radius + theme.edge.three_corner.radius_max

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
    const offsetX = Math.abs(inputs.targetOffset.x - sourceX)
    const naturalRadius = Math.min(
      Math.abs(inputs.targetOffset.x - sourceX),
      Math.abs(inputs.targetOffset.y),
    )
    let sourceDY = 0
    let startsInPort = true
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
      sourceDY = 0 - innerTheme.source_node_overlap
      startsInPort = offsetX < innerTheme.minimum_tangent_exit_radius
    }
    const source = new Vec2(sourceX, sourceDY)
    return {
      points: [source, inputs.targetOffset],
      maxRadius,
      startsInPort,
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
      startsInPort: horizontalRoomFor3CornersNoSqueeze,
    }
  }
}

type Line = { axis: 'h' | 'v'; length: number }
type Arc = { radius: number; signX: number; signY: number; sweep: 0 | 1 }
type Element = Arc | Line

/**
 * Convert calculated path junction points to an array of rounded horizontal/vertical path elements.
 */
export function pathElements(junctions: JunctionPoints): { start: Vec2; elements: Element[] } {
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

function circleIntersection(x: number, r1: number, r2: number): number {
  const xNorm = clamp(x, -r2, r1)
  return Math.sqrt(r1 * r1 + r2 * r2 - xNorm * xNorm)
}

/**
 * Convert a set of generated path elements to svg path syntax representation.
 */
export function toSvgPath(sourcePos: Vec2, elements: Element[]): string {
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
