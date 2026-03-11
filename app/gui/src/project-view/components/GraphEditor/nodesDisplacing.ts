import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { type NodeId } from '$/providers/openedProjects/graph'
import { Vec2 } from '@/util/data/vec2'
import { ref } from 'vue'

interface RectLike {
  top: number
  bottom: number
  left: number
  right: number
}

/** Composable supporting moving nodes in response to a node being resized. */
export function useNodesDisplacing() {
  const { graph, module } = useCurrentProject()

  const lastPushStart = ref<[NodeId, Quad]>()

  function displaceNodesForResize(resizedId: NodeId, rect0: RectLike, rect1: RectLike) {
    const ids: NodeId[] = []
    const rects: Quad[] = []
    for (const [id, rect] of graph.value.nodeRects.entries()) {
      if (id !== resizedId && graph.value.db.isNodeId(id)) {
        ids.push(id)
        rects.push(Quad.fromRect(rect))
      }
    }
    const bounds0 = Quad.fromRect(rect0)
    const bounds1 = Quad.fromRect(rect1)
    const pushStart = lastPushStart.value?.[0] === resizedId ? lastPushStart.value?.[1] : undefined
    const displacements = nodeDisplacements(rects, bounds0, bounds1, pushStart)
    if (!displacements) return
    const { moves, pullLimits } = displacements
    lastPushStart.value = [resizedId, pullLimits]
    module.value.batchEdits(() => {
      for (const [i, pos] of moves) graph.value.setNodePosition(ids[i]!, pos)
    })
  }

  return { displaceNodesForResize }
}

type QMask = number
function qmask(a: boolean, b: boolean, c: boolean, d: boolean): QMask {
  return +a | (+b << 1) | (+c << 2) | (+d << 3)
}
namespace QMask {
  export function invert(m: QMask): QMask {
    return ~m & 0b1111
  }
  export const X = qmask(true, true, false, false)
  export const Y = qmask(false, false, true, true)
  export const TOP = qmask(false, false, true, false)
  export const LEFT = qmask(true, false, false, false)
}

/**
 * @internal
 * A representation of a rectangle that simplifies bounds operations.
 *
 * The signs of the `left` and `top` sides are negated, so the same logic can be used when comparing `left` bounds and
 * when comparing `right` bounds: Less-than always means toward the inside of the rectangle, and adding a positive value
 * to any of the coordinates makes the rectangle bigger.
 */
export type Quad = [number, number, number, number]
/** @internal */
export namespace Quad {
  /** Conversion. */
  export function fromRect({ left, right, top, bottom }: RectLike): Quad {
    return [-left, right, -top, bottom]
  }
  /** Returns the position. */
  export function toPos([a, , c]: Quad): Vec2 {
    return new Vec2(-a, -c)
  }
  /** Returns the size. */
  export function toSize([a, b, c, d]: Quad): Vec2 {
    return new Vec2(a + b, c + d)
  }
  /** Create a Quad with all values equal. */
  export function splat(value: number): Quad {
    return [value, value, value, value]
  }
  /**
   * Reduce the values to a combined-horizontal and combined-vertical component. This doesn't make sense for a quad that
   * represents bounds, but can convert a quad that represents bounds *deltas* to a vector.
   */
  export function deltaToVec2([a, b, c, d]: Quad): Vec2 {
    return new Vec2(b - a, d - c)
  }
  /** Returns the left/top position coordinates. */
  export function pos([a, _b, c, _d]: Quad): Vec2 {
    return new Vec2(-a, -c)
  }

  /**
   * Turn the rectangle inside out.
   *
   * Directly comparing rectangles can only tell us how far inside one rectangle another rectangle is, on each side. By
   * inverting a rectangle before comparing it, we can tell how far outside one rectangle is from another on each side.
   */
  export function invert([a, b, c, d]: Quad): Quad {
    return [-b, -a, -d, -c]
  }

  /** Return a new quad, with bounds not included in the mask zeroed. */
  export function mask([a, b, c, d]: Quad, m: QMask): Quad {
    return [m & 1 ? a : 0, m & 2 ? b : 0, m & 4 ? c : 0, m & 8 ? d : 0]
  }
  /** Return a new quad, with bits in the mask selecting from the inputs. */
  export function select(q0: Quad, q1: Quad, m: QMask): Quad {
    return [
      m & 1 ? q1[0] : q0[0],
      m & 2 ? q1[1] : q0[1],
      m & 4 ? q1[2] : q0[2],
      m & 8 ? q1[3] : q0[3],
    ]
  }

  function pairwise(f: (a: number, b: number) => number): (q: Quad, r: Quad) => Quad {
    return (q: Quad, r: Quad) => [f(q[0], r[0]), f(q[1], r[1]), f(q[2], r[2]), f(q[3], r[3])]
  }
  /** Elementwise operation. */
  export const add = pairwise((a, b) => a + b)
  /** Elementwise operation. */
  export const sub = pairwise((a, b) => a - b)
  /** Elementwise operation. */
  export const min = pairwise((a, b) => Math.min(a, b))
  /** Elementwise operation. */
  export const max = pairwise((a, b) => Math.max(a, b))

  function pairwiseMask(f: (a: number, b: number) => boolean): (q: Quad, r: Quad) => QMask {
    return (q: Quad, r: Quad) => qmask(f(q[0], r[0]), f(q[1], r[1]), f(q[2], r[2]), f(q[3], r[3]))
  }
  /** Elementwise comparison. */
  export const lt = pairwiseMask((a, b) => a < b)
  /** Elementwise comparison. */
  export const lte = pairwiseMask((a, b) => a <= b)
  /** Elementwise comparison. */
  export const gt = pairwiseMask((a, b) => a > b)
  /** Elementwise comparison. */
  export const gte = pairwiseMask((a, b) => a >= b)
  /** Elementwise comparison. */
  export const eq = pairwiseMask((a, b) => a === b)
  /** Elementwise comparison. */
  export const ne = pairwiseMask((a, b) => a !== b)

  /** Zero quad. */
  export const ZERO: Quad = [0, 0, 0, 0]
}

// Distance in scene pixels at which a node is considered close enough to the resized node to be moved out of the way.
const PADDING: Quad = Quad.fromRect({ left: 32, right: 32, top: 32, bottom: 32 })

function rectsToDisplace(rects: Quad[], bounds0: Quad, bounds1: Quad, padding: Quad) {
  const changing = Quad.ne(bounds0, bounds1)
  /**
   * Nodes which are beyond the reference node in any direction of movement,
   * and the reference node bounds they are beyond.
   */
  const movable: [number, QMask][] = []
  const collisionBounds = Quad.add(bounds1, Quad.mask(padding, changing))
  /** Reference node bounds where collisions with other nodes would occur. */
  const colliding: Quad[] = []
  rects.forEach((rect, i) => {
    const bounds = Quad.invert(rect)
    const beyond0 = Quad.lte(bounds0, bounds)
    const dims = beyond0 & changing
    if (dims) {
      movable.push([i, dims])
      if (Quad.lt(collisionBounds, bounds) === 0) colliding.push(bounds)
    }
  })
  return { movable, colliding }
}

/** @internal */
export function pushStarts(
  bounds0: Quad,
  bounds1: Quad,
  colliding: Quad[],
  padding: Quad,
  pushStarts: Quad | undefined,
) {
  const expanding = Quad.lt(bounds0, bounds1)

  const boundsWithoutPush = bounds1
  const boundsWithXYPush = bounds0
  const boundsWithXPush = Quad.select(boundsWithoutPush, boundsWithXYPush, QMask.X)
  const boundsWithYPush = Quad.select(boundsWithoutPush, boundsWithXYPush, QMask.Y)

  const starts = pushStarts ?? Quad.splat(Infinity)
  // The bounds that would overlap the X-closest nodes if only Y-pushing were applied.
  let pushX = starts
  // The bounds that would overlap the Y-closest nodes if only X-pushing were applied.
  let pushY = starts
  // The bounds of the closest node that would overlap if neither X- nor Y-pushing were applied.
  let pushAny = starts
  const min = (q: Quad | null, r: Quad) => (q ? Quad.min(q, r) : r)
  for (let bounds of colliding) {
    bounds = Quad.max(bounds0, Quad.sub(bounds, Quad.mask(padding, expanding)))
    if (Quad.lt(boundsWithoutPush, bounds) !== 0) continue
    const expandingBounds = Quad.select(Quad.splat(Infinity), bounds, expanding)
    pushAny = min(pushAny, expandingBounds)
    if (Quad.lt(boundsWithXPush, bounds) === 0) pushY = min(pushY, expandingBounds)
    if (Quad.lt(boundsWithYPush, bounds) === 0) pushX = min(pushX, expandingBounds)
  }

  // If the only collision is diagonal from the reference node, a push in either direction may work; prefer Y.
  if (Quad.eq(pushX, Quad.splat(Infinity)) & QMask.X) pushY = min(pushY, pushAny)

  return Quad.select(pushX, pushY, QMask.Y)
}

/** @internal */
export function nodeDisplacements(
  rects: Quad[],
  bounds0: Quad,
  bounds1: Quad,
  pullLimits: Quad | undefined,
  padding: Quad = PADDING,
) {
  if (Quad.ne(bounds0, bounds1) & (QMask.TOP | QMask.LEFT))
    return { moves: [], pullLimits: undefined }
  if (pullLimits && Quad.lt(bounds0, pullLimits) !== Quad.eq(pullLimits, Quad.splat(Infinity)))
    pullLimits = undefined

  const analyzed = rectsToDisplace(rects, bounds0, bounds1, padding)
  const { movable, colliding } = analyzed

  // If we haven't set a pull limit (i.e. if we haven't pushed yet), don't pull
  const pull = !!pullLimits
  // When pulling, bound the final position
  if (pullLimits)
    bounds1 = Quad.select(
      bounds1,
      pullLimits,
      Quad.lt(bounds1, bounds0) & Quad.lt(bounds1, pullLimits),
    )
  pullLimits = pushStarts(bounds0, bounds1, colliding, padding, pullLimits)
  // When pushing, adjust for starting the push partway through this move
  bounds0 = Quad.select(
    bounds0,
    pullLimits,
    Quad.ne(pullLimits, Quad.splat(Infinity)) & Quad.lt(bounds0, pullLimits),
  )
  const delta = Quad.sub(bounds1, bounds0)

  const activeBounds =
    (+pull && Quad.lt(delta, Quad.ZERO)) |
    (Quad.ne(pullLimits, Quad.splat(Infinity)) & Quad.lt(Quad.ZERO, delta))
  if (!activeBounds) return undefined

  const moves: [number, Vec2][] = []
  for (const [i, sides] of movable) {
    const moveSides = sides & activeBounds
    if (moveSides) {
      const pos0 = Quad.toPos(rects[i]!)
      const pos1 = pos0.add(Quad.deltaToVec2(Quad.mask(delta, moveSides)))
      moves.push([i, pos1])
    }
  }
  return { moves, pullLimits }
}
