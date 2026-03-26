import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { type NodeId } from '$/providers/openedProjects/graph'
import { Vec2 } from '@/util/data/vec2'

/** Composable supporting moving nodes in response to a node being resized. */
export function useNodesDisplacing() {
  const { graph, module } = useCurrentProject()
  const displacing = useDisplacing()

  let lastPushStart: [NodeId, Quad] | undefined = undefined

  /**
   * Push or pull other nodes as appropriate for the given move operation. This is a frame-by-frame
   * algorithm; each step of a resize operation is analyzed independently, though some state is
   * maintained internally.
   */
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
    const pushStart = lastPushStart?.[0] === resizedId ? lastPushStart?.[1] : undefined
    const displacements = displacing.step(rects, bounds0, bounds1, pushStart)
    if (!displacements) return
    const { moves, pullLimits } = displacements
    lastPushStart = pullLimits && [resizedId, pullLimits]
    module.value.batchEdits(() => {
      for (const [i, pos] of moves) graph.value.setNodePosition(ids[i]!, pos)
    })
  }

  return { displaceNodesForResize }
}

interface RectLike {
  top: number
  bottom: number
  left: number
  right: number
}

type QMask = number
function qmask(a: boolean, b: boolean, c: boolean, d: boolean): QMask {
  return +a | (+b << 1) | (+c << 2) | (+d << 3)
}
namespace QMask {
  /**
   * Bitwise negation. This should be used instead of the ~ operator to avoid setting bits not used
   * by a QMask.
   */
  export function invert(m: QMask): QMask {
    return ~m & 0b1111
  }
  export const X = qmask(true, true, false, false)
  export const Y = qmask(false, false, true, true)
  export const TOP = qmask(false, false, true, false)
  export const LEFT = qmask(true, false, false, false)
}

/**
 * A representation of a rectangle that simplifies bounds operations.
 *
 * The signs of the `left` and `top` sides are negated, so the same logic can be used when comparing
 * `left` bounds and when comparing `right` bounds: Less-than always means toward the inside of the
 * rectangle, and adding a positive value to any of the coordinates makes the rectangle bigger.
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
   * Reduce the values to a combined-horizontal and combined-vertical component. This doesn't make
   * sense for a quad that represents bounds, but can convert a quad that represents bounds *deltas*
   * to a vector.
   */
  export function deltaToVec2([a, b, c, d]: Quad): Vec2 {
    return new Vec2(b - a, d - c)
  }

  /**
   * Turn the rectangle inside out.
   *
   * Directly comparing rectangles can only tell us how far inside one rectangle another rectangle
   * is, on each side. By inverting a rectangle before comparing it, we can tell how far outside one
   * rectangle is from another on each side.
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

// Distance in scene pixels at which a node is considered close enough to the resized node to be
// moved out of the way.
const GRAPH_NODE_PADDING: Quad = Quad.fromRect({ left: 32, right: 32, top: 32, bottom: 32 })

/** Returns a displacing API for the given configuration. (Exported for testing.) */
export function useDisplacing(config: { padding?: Quad } = {}) {
  const padding = config.padding ?? GRAPH_NODE_PADDING

  /**
   * Given a resized rectangle and a set of other rectangles, compute which input rectangles should
   * be moved if movement is necessary; and the set of rectangles that are close enough that they
   * may cause moving to be necessary.
   */
  function rectsToDisplace(
    rects: Quad[],
    bounds0: Quad,
    bounds1: Quad,
  ): {
    movable: [number, QMask][]
    colliding: Quad[]
  } {
    const changing = Quad.ne(bounds0, bounds1)
    /**
     * Nodes that are beyond the reference node in any direction of movement;
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

  /** Given a resize step and potentially colliding objects, return a pushing state. */
  function pushStarts(bounds0: Quad, bounds1: Quad, colliding: Quad[]): Quad {
    const expanding = Quad.lt(bounds0, bounds1)

    const boundsWithoutPush = bounds1
    const boundsWithXYPush = bounds0
    const boundsWithXPush = Quad.select(boundsWithoutPush, boundsWithXYPush, QMask.X)
    const boundsWithYPush = Quad.select(boundsWithoutPush, boundsWithXYPush, QMask.Y)

    // The bounds that would overlap the X-closest nodes if only Y-pushing were applied.
    let pushX = Quad.splat(Infinity)
    // The bounds that would overlap the Y-closest nodes if only X-pushing were applied.
    let pushY = Quad.splat(Infinity)
    // The bounds of the closest node that would overlap if neither X- nor Y-pushing were applied.
    let pushAny = Quad.splat(Infinity)
    for (let bounds of colliding) {
      bounds = Quad.max(bounds0, Quad.sub(bounds, Quad.mask(padding, expanding)))
      if (Quad.lt(boundsWithoutPush, bounds) !== 0) continue
      const expandingBounds = Quad.select(Quad.splat(Infinity), bounds, expanding)
      pushAny = Quad.min(pushAny, expandingBounds)
      if (Quad.lt(boundsWithXPush, bounds) === 0) pushY = Quad.min(pushY, expandingBounds)
      if (Quad.lt(boundsWithYPush, bounds) === 0) pushX = Quad.min(pushX, expandingBounds)
    }

    // If the only collision is diagonal from the reference node, a push in either direction may
    // work; prefer Y.
    if (!(Quad.ne(pushX, Quad.splat(Infinity)) & QMask.X)) pushY = Quad.min(pushY, pushAny)

    return Quad.select(pushX, pushY, QMask.Y)
  }

  /**
   * Given a resize step for a rectangle, a set of other rectangles, and information about the
   * resize history of the resized rectangle, return how the other rectangles should be moved, and
   * an updated resize history.
   */
  function step(
    rects: Quad[],
    bounds0: Quad,
    bounds1: Quad,
    pullLimits: Quad | undefined,
  ):
    | {
        moves: [number, Vec2][]
        pullLimits: Quad
      }
    | undefined {
    if (Quad.ne(bounds0, bounds1) & (QMask.TOP | QMask.LEFT)) return undefined
    if (pullLimits && Quad.lt(bounds0, pullLimits) !== Quad.eq(pullLimits, Quad.splat(Infinity)))
      pullLimits = undefined

    const analyzed = rectsToDisplace(rects, bounds0, bounds1)
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
    const newPushStarts = pushStarts(bounds0, bounds1, colliding)
    pullLimits = Quad.min(newPushStarts, pullLimits ?? Quad.splat(Infinity))
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

  return { step, pushStarts }
}
