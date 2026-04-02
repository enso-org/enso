import { pushStarts, Quad, step } from '@/components/GraphEditor/nodesDisplacing'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { fc, test as fcTest } from '@fast-check/vitest'
import { describe, expect, test } from 'vitest'

interface StaticCase {
  rects: Quad[]
  bounds0: Quad
}

interface ResizeCase extends StaticCase {
  bounds1: Quad
  padding?: Quad
}

const UNIT = { top: 0, left: 0, right: 1, bottom: 1 }

const EXPANDING_BOTTOM: ResizeCase = {
  rects: [Quad.fromRect({ ...UNIT, top: 2, bottom: 3 })],
  bounds0: Quad.fromRect(UNIT),
  bounds1: Quad.fromRect({ ...UNIT, bottom: 3 }),
}
const EXPANDING_RIGHT: ResizeCase = {
  rects: [Quad.fromRect({ ...UNIT, left: 2, right: 3 })],
  bounds0: Quad.fromRect(UNIT),
  bounds1: Quad.fromRect({ ...UNIT, right: 3 }),
}
const EXPANDING_BOTTOM_RIGHT: ResizeCase = {
  rects: [Quad.fromRect({ left: 2, right: 3, top: 2, bottom: 3 })],
  bounds0: Quad.fromRect(UNIT),
  bounds1: Quad.fromRect({ ...UNIT, right: 3, bottom: 3 }),
}
const EXPANDING_BOTTOM_RIGHT_AT_PADDING: ResizeCase = {
  rects: [Quad.fromRect({ left: 2, right: 3, top: 2, bottom: 3 })],
  bounds0: Quad.fromRect(UNIT),
  bounds1: Quad.fromRect({ ...UNIT, right: 3, bottom: 3 }),
  padding: Quad.splat(1),
}
const EXPANDING_BOTTOM_RIGHT_WITHIN_PADDING: ResizeCase = {
  rects: [Quad.fromRect({ left: 2, right: 3, top: 2, bottom: 3 })],
  bounds0: Quad.fromRect(UNIT),
  bounds1: Quad.fromRect({ ...UNIT, right: 3, bottom: 3 }),
  padding: Quad.splat(1.5),
}
const EXPANDING_BOTTOM_FAR: ResizeCase = {
  rects: [Quad.fromRect({ ...UNIT, top: 2, bottom: 3 })],
  bounds0: Quad.fromRect(UNIT),
  bounds1: Quad.fromRect({ ...UNIT, bottom: 4 }),
}
const EXPANDING_BOTTOM_PADDED: ResizeCase = {
  ...EXPANDING_BOTTOM,
  padding: Quad.splat(1),
}
const EXPANDING_BOTTOM_PADDING_EXCEEDS_INITIAL_DISTANCE: ResizeCase = {
  ...EXPANDING_BOTTOM,
  padding: Quad.splat(2),
}

/** Tests for the building blocks of the step function. */
describe('Internal function tests', () => {
  test.each([
    { input: EXPANDING_BOTTOM, expected: [Infinity, Infinity, Infinity, 2] },
    { input: EXPANDING_RIGHT, expected: [Infinity, 2, Infinity, Infinity] },
    { input: EXPANDING_BOTTOM_RIGHT, expected: [Infinity, 2, Infinity, 2] },
    {
      input: {
        rects: [[-1, 2, -5, 6] satisfies Quad],
        bounds0: [2, 4, 2, 4] satisfies Quad,
        bounds1: [2, 9, 2, 9] satisfies Quad,
        padding: Quad.splat(1),
      },
      expected: [Infinity, Infinity, Infinity, 5],
    },
  ])('pushStarts', ({ input: { rects, bounds0, bounds1 }, expected }) => {
    expect(pushStarts(bounds0, bounds1, rects.map(Quad.invert))).toEqual(expected)
  })
})

/**
 * Tests that verify the step function exhibits desired properties; tests that confirm expected
 * behaviour in known edge cases.
 */
describe('Step function tests', () => {
  test.each([
    {
      input: EXPANDING_BOTTOM,
      expected: {
        moves: [[0, new Vec2(0, 3)]],
      },
    },
    {
      input: EXPANDING_RIGHT,
      expected: {
        moves: [[0, new Vec2(3, 0)]],
      },
    },
    {
      input: EXPANDING_BOTTOM_RIGHT,
      expected: {
        moves: [[0, new Vec2(3, 3)]],
      },
    },
    {
      input: EXPANDING_BOTTOM_RIGHT_AT_PADDING,
      expected: {
        moves: [[0, new Vec2(4, 4)]],
      },
    },
    {
      input: EXPANDING_BOTTOM_RIGHT_WITHIN_PADDING,
      expected: {
        moves: [[0, new Vec2(4, 4)]],
      },
    },
    {
      input: EXPANDING_BOTTOM_FAR,
      expected: {
        moves: [[0, new Vec2(0, 4)]],
      },
    },
    {
      input: EXPANDING_BOTTOM_PADDED,
      expected: {
        moves: [[0, new Vec2(0, 4)]],
      },
    },
    {
      input: EXPANDING_BOTTOM_PADDING_EXCEEDS_INITIAL_DISTANCE,
      expected: {
        moves: [[0, new Vec2(0, 4)]],
      },
    },
    {
      input: {
        rects: [Quad.fromRect({ ...UNIT, left: 4, right: 5 })],
        bounds0: Quad.fromRect(UNIT),
        bounds1: Quad.fromRect({ ...UNIT, bottom: 10, right: 10 }),
      },
      expected: {
        moves: [[0, new Vec2(10, 0)]],
      },
    },
    {
      input: {
        rects: [Quad.fromRect({ top: 3, bottom: 4, left: 4, right: 5 })],
        bounds0: Quad.fromRect(UNIT),
        bounds1: Quad.fromRect({ ...UNIT, bottom: 10, right: 10 }),
        padding: Quad.splat(1),
      },
      expected: {
        moves: [[0, new Vec2(11, 11)]],
      },
    },
  ])('Displacing step', ({ input: { rects, bounds0, bounds1, padding }, expected }) => {
    const moves = step(rects, bounds0, bounds1, undefined, padding ?? Quad.ZERO)?.moves ?? []
    expect(moves).toEqual(expected.moves)
  })

  // Test the critical property for pushes: They move the other nodes enough to prevent collision,
  // but not farther.
  fcTest.prop({
    pos: fc.float({ min: 1, max: 5, noNaN: true }),
    push: fc.float({ min: 0, minExcluded: true, max: 5, noNaN: true }),
  })('Node kept beyond reference, not pushed beyond padding', ({ pos, push }) => {
    const PADDING = 1
    const displacements = step(
      [Quad.fromRect({ ...UNIT, top: pos, bottom: pos + 1 })],
      Quad.fromRect(UNIT),
      Quad.fromRect({ ...UNIT, bottom: push + 1 }),
      undefined,
      Quad.splat(PADDING),
    )
    const moves = displacements?.moves ?? []
    const pos1 = moves[0] ? moves[0][1].y : pos
    expect(pos1).toBeGreaterThanOrEqual(push + 1)
    if (moves[0]) expect(pos1 - (push + 1)).toBeLessThan(PADDING + 0.1)
  })
})

/**
 * Tests that verify the step function exhibits desired properties when repeated/applied to random
 * graphs.
 */
describe('Integration tests', () => {
  class DisplacementGraph {
    pullLimits: Quad | undefined = undefined
    constructor(
      public nodes: Quad[],
      private padding: Quad,
    ) {}

    step(from: Quad, to: Quad) {
      const action = step(this.nodes, from, to, this.pullLimits, this.padding)
      if (!action) return
      const { moves, pullLimits } = action
      this.pullLimits = pullLimits
      for (const [i, pos] of moves) {
        const size = Quad.toSize(this.nodes[i]!)
        const bottomRight = pos.add(size)
        this.nodes[i] = Quad.fromRect({
          top: pos.y,
          left: pos.x,
          right: bottomRight.x,
          bottom: bottomRight.y,
        })
      }
    }
  }

  const unitSquareAt = (pos: Vec2) =>
    Quad.fromRect({ left: pos.x, top: pos.y, right: pos.x + 1, bottom: pos.y + 1 })

  const approxVec = ({ x, y }: { x: number; y: number }) => ({
    x: expect.closeTo(x),
    y: expect.closeTo(y),
  })

  const randomGraph = fc.array(
    fc.record({
      top: fc.float({ min: 1, max: 10, noNaN: true }),
      left: fc.float({ min: 1, max: 10, noNaN: true }),
    }),
    {
      minLength: 1,
      maxLength: 100,
    },
  )

  interface PushPullTestCase {
    nodes: { top: number; left: number }[]
  }

  function testPushPull({ nodes }: PushPullTestCase) {
    const PADDING = Quad.splat(1)
    const resizedNodePos = new Vec2(-2, -2)
    const initialSize = new Vec2(1, 1)
    const pushDistance = new Vec2(10, 10)
    const initialNodes = nodes.map((node) => new Vec2(node.left, node.top))

    const graph = new DisplacementGraph(initialNodes.map(unitSquareAt), PADDING)
    const rect0 = Quad.fromRect(new Rect(resizedNodePos, initialSize))
    const rect1 = Quad.fromRect(new Rect(resizedNodePos, initialSize.add(pushDistance)))
    graph.step(rect0, rect1)
    graph.step(rect1, rect0)
    expect(graph.nodes.map((node) => Quad.toPos(node).xy())).toEqual(initialNodes.map(approxVec))
  }

  // Test the critical property for pulls: They undo pushes.
  test.each([
    {
      nodes: [
        { top: 1, left: 9 },
        { top: 1, left: 1 },
      ],
    },
    {
      nodes: [{ top: 1, left: 1 }],
    },
    {
      nodes: [
        { top: 2.000000238418579, left: 1 },
        { top: 1, left: 2.000000238418579 },
      ],
    },
  ])('Push-pull cases', testPushPull)
  fcTest.prop({
    nodes: randomGraph,
  })('Push-then-pull returns nodes to original positions', testPushPull)
})
