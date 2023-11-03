import { partitionPoint } from '@/util/array'
import type { Opt } from '@/util/opt'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { abs } from 'lib0/math'
import type { ExprId } from 'shared/yjsModel'
import { computed, markRaw, type ComputedRef } from 'vue'

export class SnapGrid {
  leftAxes: number[]
  rightAxes: number[]
  topAxes: number[]
  bottomAxes: number[]

  constructor(rects: Rect[]) {
    markRaw(this)
    this.leftAxes = Array.from(rects, (rect) => rect.left).sort((a, b) => a - b)
    this.rightAxes = Array.from(rects, (rect) => rect.right).sort((a, b) => a - b)
    this.topAxes = Array.from(rects, (rect) => rect.top).sort((a, b) => a - b)
    this.bottomAxes = Array.from(rects, (rect) => rect.bottom).sort((a, b) => a - b)
    console.log(this.leftAxes)
    console.log(this.rightAxes)
    console.log(this.topAxes)
    console.log(this.bottomAxes)
  }

  snappedMany(rects: Rect[], threshold: number): Vec2 {
    const minSnap = rects.reduce<[number | null, number | null]>(
      (minSnap, rect) => {
        const [xSnap, ySnap] = this.snap(rect, threshold)
        return [SnapGrid.minSnap(minSnap[0], xSnap), SnapGrid.minSnap(minSnap[1], ySnap)]
      },
      [null, null],
    )
    return new Vec2(minSnap[0] ?? 0.0, minSnap[1] ?? 0.0)
  }

  snap(rect: Rect, threshold: number): [number | null, number | null] {
    const leftSnap = SnapGrid.boundSnap(rect.left, this.leftAxes, threshold)
    const rightSnap = SnapGrid.boundSnap(rect.right, this.rightAxes, threshold)
    const topSnap = SnapGrid.boundSnap(rect.top, this.topAxes, threshold)
    const bottomSnap = SnapGrid.boundSnap(rect.bottom, this.bottomAxes, threshold)
    return [SnapGrid.minSnap(leftSnap, rightSnap), SnapGrid.minSnap(topSnap, bottomSnap) ?? 0.0]
  }

  private static boundSnap(value: number, axes: number[], threshold: number): number | null {
    const firstNotLower = partitionPoint(axes, (x) => x <= value)
    const lowerNearest = axes[firstNotLower - 1]
    const lowerSnap = lowerNearest != null ? lowerNearest - value : null
    const notLowerNearest = axes[firstNotLower]
    const higherSnap = notLowerNearest != null ? notLowerNearest - value : null
    const snap = SnapGrid.minSnap(lowerSnap, higherSnap)
    return snap == null ? snap : abs(snap) <= threshold ? snap : null
  }
  private static minSnap(a: Opt<number>, b: Opt<number>): number | null {
    if (a != null && b != null) return abs(a) < abs(b) ? a : b
    else if (a != null) return a
    else if (b != null) return b
    else return null
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

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
  ])('Snapping to single rect: $name', ({ snappedRects, expected }) => {
    const rects = [Rect.fromBounds(-5.0, -5.0, 15.0, 5.0)]
    const grid = new SnapGrid(rects)

    const converted = Array.from(snappedRects, ([l, t, r, b]) => Rect.fromBounds(l!, t!, r!, b!))
    const snap = grid.snappedMany(converted, 1.0)
    expect(snap.x).toBeCloseTo(expected[0]!)
    expect(snap.y).toBeCloseTo(expected[1]!)
  })

  test.each`
    snappedRectX | expectedXSnap
    ${-1000.0}   | ${null}
    ${-11.0}     | ${1.0}
    ${-9.7}      | ${-0.3}
    ${-9.4}      | ${0.4}
    ${-0.6}      | ${-0.4}
    ${-0.4}      | ${0.4}
    ${0.0}       | ${0.0}
    ${0.4}       | ${-0.4}
    ${0.6}       | ${0.4}
    ${9.4}       | ${-0.4}
    ${9.7}       | ${0.3}
    ${11.0}      | ${-1.0}
    ${1000.0}    | ${null}
  `(
    'Snapping rect with left $snappedRectX to the nearest boundary',
    ({ snappedRectX, expectedXSnap }) => {
      const rects = []
      for (let x = -10.0; x <= 10.1; x += 1.0) {
        rects.push(Rect.fromBounds(x, 0.0, 100.0, 10.0))
      }
      const grid = new SnapGrid(rects)
      const snapped = new Rect(new Vec2(snappedRectX, 0.0), new Vec2(10.0, 10.0))
      expect(grid.snap(snapped, 15.0)[0]).toBeCloseTo(expectedXSnap)
    },
  )
}
