import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { useApproach } from '@/util/animation'
import { partitionPoint } from '@/util/array'
import type { Opt } from '@/util/opt'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { iteratorFilter } from 'lib0/iterator'
import { abs } from 'lib0/math'
import type { ExprId } from 'shared/yjsModel'
import { computed, markRaw, ref, watchEffect, type WatchStopHandle } from 'vue'

const DRAG_SNAP_THRESHOLD = 15

export class SnapGrid {
  leftAxes: number[]
  rightAxes: number[]
  topAxes: number[]
  bottomAxes: number[]

  constructor(rects: Rect[]) {
    this.leftAxes = Array.from(rects, (rect) => rect.left).sort((a, b) => a - b)
    this.rightAxes = Array.from(rects, (rect) => rect.right).sort((a, b) => a - b)
    this.topAxes = Array.from(rects, (rect) => rect.top).sort((a, b) => a - b)
    this.bottomAxes = Array.from(rects, (rect) => rect.bottom).sort((a, b) => a - b)
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

/**
 * Composable handling dragging nodes, including snapping them to axes determined by other nodes'
 * boundaries (to make node aligning easier for the user).
 *
 * The set of dragged nodes is determined upon construction. The new position is stored in the
 * node metadata only when dragging is finished.
 *
 * The `offset` is always based of the nodes' initial positions.
 */
export function useDragging() {
  const graphStore = useGraphStore()
  const selection = injectGraphSelection(true)

  // Logically, those fields could be inside CurrentDrag, but animations need component scope.
  const offset = ref(Vec2.Zero)
  const snapXTarget = ref(0)
  const snapX = useApproach(snapXTarget, 30)
  const snapYTarget = ref(0)
  const snapY = useApproach(snapYTarget, 30)
  const snappedOffset = computed(() => offset.value.add(new Vec2(snapX.value, snapY.value)))
  const snappedOffsetTarget = computed(() =>
    offset.value.add(new Vec2(snapXTarget.value, snapYTarget.value)),
  )
  class CurrentDrag {
    draggedNodes: ExprId[]
    grid: SnapGrid
    stopPositionUpdate: WatchStopHandle

    constructor(movedId: ExprId) {
      markRaw(this)
      this.draggedNodes = selection?.isSelected(movedId)
        ? Array.from(selection.selected)
        : [movedId]
      this.grid = this.createSnapGrid()
      offset.value = Vec2.Zero
      snapXTarget.value = 0
      snapYTarget.value = 0
      snapX.skip()
      snapY.skip()

      this.stopPositionUpdate = watchEffect(() => {
        for (const id of this.draggedNodes) {
          const node = graphStore.nodes.get(id)
          if (node == null) continue
          node.visiblePosition = node.position.add(snappedOffset.value)
        }
      })
    }

    updateOffset(newOffset: Vec2): void {
      const oldSnappedOffset = snappedOffset.value
      const rects: Rect[] = []
      for (const id of this.draggedNodes) {
        const rect = graphStore.nodeRects.get(id)
        const node = graphStore.nodes.get(id)
        if (rect != null && node != null)
          rects.push(new Rect(node.position.add(newOffset), rect.size))
      }
      const snap = this.grid.snappedMany(rects, DRAG_SNAP_THRESHOLD)
      offset.value = newOffset
      snapXTarget.value = snap.x
      snapYTarget.value = snap.y
      const newSnappedOffsetTarget = snappedOffsetTarget.value
      // Skip animation if target offset does not change significantly, to avoid shivering
      // when node is snapped.
      if (abs(newSnappedOffsetTarget.x - oldSnappedOffset.x) < 2.0) {
        snapX.skip()
      }
      if (abs(newSnappedOffsetTarget.y - oldSnappedOffset.y) < 2.0) {
        snapY.skip()
      }
    }

    finishDragging(): void {
      this.stopPositionUpdate()
      for (const id of this.draggedNodes) {
        const node = graphStore.nodes.get(id)
        if (node == null || node.visiblePosition == null) continue
        const newPosition = node.position.add(snappedOffsetTarget.value)
        graphStore.setNodePosition(id, newPosition)
        node.visiblePosition = undefined
      }
    }

    createSnapGrid() {
      const excludeSet = new Set<ExprId>(this.draggedNodes)
      const withoutExcluded = iteratorFilter(
        graphStore.nodeRects.entries(),
        ([id]) => !excludeSet.has(id),
      )
      return new SnapGrid(Array.from(withoutExcluded, ([, rect]) => rect))
    }
  }
  let currentDrag: CurrentDrag | undefined

  return {
    startOrUpdate(movedId: ExprId, offset: Vec2) {
      currentDrag ??= new CurrentDrag(movedId)
      currentDrag.updateOffset(offset)
    },
    finishDrag() {
      currentDrag?.finishDragging()
      currentDrag = undefined
    },
  }
}
