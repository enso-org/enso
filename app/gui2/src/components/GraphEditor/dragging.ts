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

const graphStore = useGraphStore()

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
    return [SnapGrid.minSnap(leftSnap, rightSnap), SnapGrid.minSnap(topSnap, bottomSnap) ?? 0.0]
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

/**
 * Class handling dragging nodes, including snapping them to axes determined by other nodes'
 * boundaries (to make node aligning easier for the user).
 *
 * The `offset` is always based of the nodes' initial positions.
 */
export class Drag {
  static THRESHOLD = 15

  draggedNodes: ExprId[]
  grid: SnapGrid
  offset = ref(Vec2.Zero)
  snapXTarget = ref(0)
  snapX = useApproach(this.snapXTarget, 30)
  snapYTarget = ref(0)
  snapY = useApproach(this.snapYTarget, 30)
  stopPositionUpdate: WatchStopHandle

  constructor(movedId: ExprId, selection?: SelectionComposable<ExprId>) {
    this.draggedNodes = selection?.isSelected(movedId) ? Array.from(selection.selected) : [movedId]
    this.grid = this.createSnapGrid()

    this.stopPositionUpdate = watchEffect(() => {
      for (const id of this.draggedNodes) {
        const node = graphStore.nodes.get(id)
        if (node == null) continue
        node.visiblePosition = node.position
          .add(this.offset.value)
          .add(new Vec2(this.snapX.value, this.snapY.value))
      }
    })
  }

  updateOffset(offset: Vec2): void {
    const oldSnappedOffset = this.offset.value.add(new Vec2(this.snapX.value, this.snapY.value))
    this.offset.value = offset
    const rects: Rect[] = []
    for (const id of this.draggedNodes) {
      const rect = graphStore.nodeRects.get(id)
      const node = graphStore.nodes.get(id)
      if (rect != null && node != null) rects.push(new Rect(node.position.add(offset), rect.size))
    }
    const snap = this.grid.snappedMany(rects, Drag.THRESHOLD)
    const newSnappedOffset = offset.add(snap)
    this.snapXTarget.value = snap.x
    if (abs(newSnappedOffset.x - oldSnappedOffset.x) < 2.0) {
      this.snapX.skip()
    }
    this.snapYTarget.value = snap.y
    if (abs(newSnappedOffset.y - oldSnappedOffset.y) < 2.0) {
      this.snapY.skip()
    }
  }

  finishDragging(): void {
    this.stopPositionUpdate()
    for (const id of this.draggedNodes) {
      const node = graphStore.nodes.get(id)
      if (node == null || node.visiblePosition == null) continue
      const newPosition = node.position
        .add(this.offset.value)
        .add(new Vec2(this.snapXTarget.value, this.snapYTarget.value))
      console.log('New Position', newPosition)
      graphStore.setNodePosition(id, newPosition)
      node.visiblePosition = undefined
    }
  }

  private createSnapGrid() {
    const excludeSet = new Set<ExprId>(this.draggedNodes)
    const withoutExcluded = iteratorFilter(
      graphStore.nodeRects.entries(),
      ([id]) => !excludeSet.has(id),
    )
    return new SnapGrid(Array.from(withoutExcluded, ([, rect]) => rect))
  }
}
