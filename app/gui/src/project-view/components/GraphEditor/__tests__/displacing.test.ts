import { Quad, nodeDisplacements, pushStarts } from '@/components/GraphEditor/nodesDisplacing'
import { Vec2 } from '@/util/data/vec2'
import { fc, test as fcTest } from '@fast-check/vitest'
import { expect, test } from 'vitest'

interface StaticCase {
  rects: Quad[]
  bounds0: Quad
}

interface ResizeCase extends StaticCase {
  bounds1: Quad
  padding?: Quad
}

const UNIT = { top: 0, left: 0, right: 1, bottom: 1 }
const INFINITE = { top: -Infinity, left: -Infinity, right: Infinity, bottom: Infinity }

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

test.each([
  { input: EXPANDING_BOTTOM, expected: [Infinity, Infinity, Infinity, 2] },
  { input: EXPANDING_RIGHT, expected: [Infinity, 2, Infinity, Infinity] },
  { input: EXPANDING_BOTTOM_RIGHT, expected: [Infinity, Infinity, Infinity, 2] },
])('pushStarts', ({ input: { rects, bounds0, bounds1, padding }, expected }) => {
  expect(
    pushStarts(bounds0, bounds1, rects.map(Quad.invert), padding ?? Quad.ZERO, undefined),
  ).toEqual(expected)
})

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
      // In this case moving X or Y will suffice; we prefer moving Y.
      moves: [[0, new Vec2(2, 3)]],
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
])('nodeDisplacements', ({ input: { rects, bounds0, bounds1, padding }, expected }) => {
  const moves =
    nodeDisplacements(rects, bounds0, bounds1, undefined, padding ?? Quad.ZERO)?.moves ?? []
  expect(moves).toEqual(expected.moves)
})

fcTest.prop({
  pos: fc.float({ min: 1, max: 5, noNaN: true }),
  push: fc.float({ min: 0, minExcluded: true, max: 5, noNaN: true }),
})('Node kept beyond reference, not pushed beyond padding', ({ pos, push }) => {
  const PADDING = 1
  const displacements = nodeDisplacements(
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
