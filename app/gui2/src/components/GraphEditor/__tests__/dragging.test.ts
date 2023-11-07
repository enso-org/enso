import { SnapGrid } from '@/components/GraphEditor/dragging'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { expect, test } from 'vitest'
import { computed } from 'vue'

test.each([
  {
    name: 'already snapped',
    snappedRects: [[-5.0, -5.0, 0.0, 0.0]],
    expected: [0.0, 0.0],
  },
  {
    name: 'left-top corner left-top snap',
    snappedRects: [[-4.5, -4.8, 0.5, 0.2]],
    expected: [-0.5, -0.2],
  },
  {
    name: 'left-top corner right-bottom snap',
    snappedRects: [[-5.4, -5.5, -0.4, -0.5]],
    expected: [0.4, 0.5],
  },
  {
    name: 'right-bottom corner left-top snap',
    snappedRects: [[10.3, 0.1, 15.3, 5.1]],
    expected: [-0.3, -0.1],
  },
  {
    name: 'right-bottom corner right-bottom snap',
    snappedRects: [[9.8, -0.5, 14.8, 4.5]],
    expected: [0.2, 0.5],
  },
  {
    name: 'right and bottom edges are not snapped to left and top edges',
    snappedRects: [[-10.2, -10.2, -5.2, -5.2]],
    expected: [0.0, 0.0],
  },
  {
    name: 'far left is not snapped',
    snappedRects: [[-20.0, -5.0, -15.0, 5.0]],
    expected: [0.0, 0.0],
  },
  {
    name: 'far bottom is not snapped',
    snappedRects: [[-5.0, 20.0, 15.0, 25.0]],
    expected: [0.0, 0.0],
  },
  {
    name: 'more than one node',
    snappedRects: [
      [-4.5, -4.8, 0.5, 0.2],
      [9.8, -0.5, 14.8, 4.5],
    ],
    expected: [0.2, -0.2],
  },
  {
    name: 'one node snapped and other not',
    snappedRects: [
      [-4.5, -4.8, 0.5, 0.2],
      [1000.0, 1000.0, 1010.0, 1010.0],
    ],
    expected: [-0.5, -0.2],
  },
])('Snapping to single rect: $name', ({ snappedRects, expected }) => {
  const rects = computed(() => [Rect.FromBounds(-5.0, -5.0, 15.0, 5.0)])
  const grid = new SnapGrid(rects)

  const converted = Array.from(snappedRects, ([l, t, r, b]) => Rect.FromBounds(l!, t!, r!, b!))
  const snap = grid.snappedMany(converted, 1.0)
  expect(snap.x).toBeCloseTo(expected[0]!)
  expect(snap.y).toBeCloseTo(expected[1]!)
})

test.each`
  snappedRectPosition | expectedSnap
  ${-1000}            | ${null}
  ${-11}              | ${1}
  ${-9.7}             | ${-0.3}
  ${-9.4}             | ${0.4}
  ${-0.6}             | ${-0.4}
  ${-0.4}             | ${0.4}
  ${0}                | ${0}
  ${0.4}              | ${-0.4}
  ${0.6}              | ${0.4}
  ${9.4}              | ${-0.4}
  ${9.7}              | ${0.3}
  ${11}               | ${-1}
  ${1000}             | ${null}
`(
  'Snapping rect with left/top $snappedRectX to the nearest boundary',
  ({ snappedRectPosition, expectedSnap }) => {
    const rects: Rect[] = []
    for (let xy = -10.0; xy <= 10.1; xy += 1.0) {
      rects.push(Rect.FromBounds(xy, 0.0, 100.0, 10.0))
      rects.push(Rect.FromBounds(0.0, xy, 100.0, 10.0))
    }
    const grid = new SnapGrid(computed(() => rects))
    const xSnapped = new Rect(new Vec2(snappedRectPosition, 0.0), new Vec2(10.0, 10.0))
    expect(grid.snap(xSnapped, 15.0)[0]).toBeCloseTo(expectedSnap)
    const ySnapped = new Rect(new Vec2(0.0, snappedRectPosition), new Vec2(10.0, 10.0))
    expect(grid.snap(ySnapped, 15.0)[1]).toBeCloseTo(expectedSnap)
  },
)
