import { useGraphStore } from '@/stores/graph'
import { useApproach } from '@/util/animation'
import { partitionPoint } from '@/util/array'
import type { Opt } from '@/util/opt'
import { Rect } from '@/util/rect'
import type { SelectionComposable } from '@/util/selection'
import { Vec2 } from '@/util/vec2'
import { iteratorFilter } from 'lib0/iterator'
import { abs } from 'lib0/math'
import type { ExprId } from 'shared/yjsModel'
import { markRaw, ref, watchEffect, type WatchStopHandle } from 'vue'

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
    const minSnap = rects.reduce<[x: number | null, y: number | null]>(
      (minSnap, rect) => {
        const [xSnap, ySnap] = this.snap(rect, threshold)
        return [SnapGrid.minSnap(minSnap[0], xSnap), SnapGrid.minSnap(minSnap[1], ySnap)]
      },
      [null, null],
    )
    return new Vec2(minSnap[0] ?? 0.0, minSnap[1] ?? 0.0)
  }

  snap(rect: Rect, threshold: number): [x: number | null, y: number | null] {
    const leftSnap = SnapGrid.boundSnap(rect.left, this.leftAxes, threshold)
    const rightSnap = SnapGrid.boundSnap(rect.right, this.rightAxes, threshold)
    const topSnap = SnapGrid.boundSnap(rect.top, this.topAxes, threshold)
    const bottomSnap = SnapGrid.boundSnap(rect.bottom, this.bottomAxes, threshold)
    return [SnapGrid.minSnap(leftSnap, rightSnap), SnapGrid.minSnap(topSnap, bottomSnap)]
  }

  private static boundSnap(value: number, axes: number[], threshold: number): number | null {
    const firstNotLower = partitionPoint(axes, (x) => x <= value)
    const lowerNearest = axes[firstNotLower - 1]
    const snapToLower = lowerNearest != null ? lowerNearest - value : null
    const notLowerNearest = axes[firstNotLower]
    const snapToHigher = notLowerNearest != null ? notLowerNearest - value : null
    const snap = SnapGrid.minSnap(snapToLower, snapToHigher)
    return snap != null && abs(snap) <= threshold ? snap : null
  }

  private static minSnap(a: Opt<number>, b: Opt<number>): number | null {
    if (a != null && b != null) return abs(a) < abs(b) ? a : b
    else return a ?? b ?? null
  }
}
