import { useApproach } from '@/composables/animation'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { partitionPoint } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import theme from '@/util/theme.json'
import { iteratorFilter } from 'lib0/iterator'
import { computed, markRaw, ref, watchEffect, type ComputedRef, type WatchStopHandle } from 'vue'

const DRAG_SNAP_THRESHOLD = 16
const VERTICAL_GAP = theme.node.vertical_gap

interface PartialVec2 {
  x: number | null
  y: number | null
}

export class SnapGrid {
  leftAxes: ComputedRef<number[]>
  rightAxes: ComputedRef<number[]>
  topAxes: ComputedRef<number[]>
  bottomAxes: ComputedRef<number[]>

  constructor(rects: ComputedRef<Rect[]>) {
    markRaw(this)
    this.leftAxes = computed(() =>
      Array.from(rects.value, (rect) => rect.left).sort((a, b) => a - b),
    )
    this.rightAxes = computed(() =>
      Array.from(rects.value, (rect) => rect.right).sort((a, b) => a - b),
    )
    this.topAxes = computed(() =>
      Array.from(rects.value, (rect) => rect.top)
        .concat(Array.from(rects.value, (rect) => rect.bottom + VERTICAL_GAP))
        .sort((a, b) => a - b),
    )
    this.bottomAxes = computed(() =>
      Array.from(rects.value, (rect) => rect.bottom)
        .concat(Array.from(rects.value, (rect) => rect.top - VERTICAL_GAP))
        .sort((a, b) => a - b),
    )
  }

  snappedMany(rects: Rect[], threshold: number): Vec2 {
    const minSnap = rects.reduce<PartialVec2>(
      (minSnap, rect) => {
        const snap = this.snap(rect, threshold)
        return { x: SnapGrid.minSnap(minSnap.x, snap.x), y: SnapGrid.minSnap(minSnap.y, snap.y) }
      },
      { x: null, y: null },
    )
    return new Vec2(minSnap.x ?? 0.0, minSnap.y ?? 0.0)
  }

  snap(rect: Rect, threshold: number): PartialVec2 {
    const leftSnap = SnapGrid.boundSnap(rect.left, this.leftAxes.value, threshold)
    const rightSnap = SnapGrid.boundSnap(rect.right, this.rightAxes.value, threshold)
    const topSnap = SnapGrid.boundSnap(rect.top, this.topAxes.value, threshold)
    const bottomSnap = SnapGrid.boundSnap(rect.bottom, this.bottomAxes.value, threshold)
    return { x: SnapGrid.minSnap(leftSnap, rightSnap), y: SnapGrid.minSnap(topSnap, bottomSnap) }
  }

  private static boundSnap(value: number, axes: number[], threshold: number): number | null {
    const firstNotLower = partitionPoint(axes, (x) => x <= value)
    const lowerNearest = axes[firstNotLower - 1]
    const snapToLower = lowerNearest != null ? lowerNearest - value : null
    const notLowerNearest = axes[firstNotLower]
    const snapToHigher = notLowerNearest != null ? notLowerNearest - value : null
    const snap = SnapGrid.minSnap(snapToLower, snapToHigher)
    return snap != null && Math.abs(snap) <= threshold ? snap : null
  }

  private static minSnap(a: Opt<number>, b: Opt<number>): number | null {
    if (a != null && b != null) return Math.abs(a) < Math.abs(b) ? a : b
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

  interface DraggedNode {
    initialPos: Vec2
    currentPos: Vec2
  }
  class CurrentDrag {
    draggedNodes: Map<NodeId, DraggedNode>
    grid: SnapGrid
    stopPositionUpdate: WatchStopHandle

    constructor(movedId: NodeId) {
      markRaw(this)
      function* draggedNodes(): Generator<[NodeId, DraggedNode]> {
        const ids = selection?.isSelected(movedId) ? selection.selected : [movedId]
        for (const id of ids) {
          const node = graphStore.db.nodeIdToNode.get(id)
          if (node != null) yield [id, { initialPos: node.position, currentPos: node.position }]
        }
      }
      this.draggedNodes = new Map(draggedNodes())
      this.grid = this.createSnapGrid()
      offset.value = Vec2.Zero
      snapXTarget.value = 0
      snapYTarget.value = 0
      snapX.skip()
      snapY.skip()

      this.stopPositionUpdate = watchEffect(() => this.updateNodesPosition())
    }

    updateOffset(newOffset: Vec2): void {
      const oldSnappedOffset = snappedOffset.value
      const rects: Rect[] = []
      for (const [id, { initialPos }] of this.draggedNodes) {
        const rect = graphStore.nodeRects.get(id)
        const node = graphStore.db.nodeIdToNode.get(id)
        if (rect != null && node != null) rects.push(new Rect(initialPos.add(newOffset), rect.size))
      }
      const snap = this.grid.snappedMany(rects, DRAG_SNAP_THRESHOLD)
      offset.value = newOffset
      snapXTarget.value = snap.x
      snapYTarget.value = snap.y
      const newSnappedOffsetTarget = snappedOffsetTarget.value
      // Skip animation if target offset does not change significantly, to avoid shivering
      // when node is snapped.
      if (Math.abs(newSnappedOffsetTarget.x - oldSnappedOffset.x) < 2.0) {
        snapX.skip()
      }
      if (Math.abs(newSnappedOffsetTarget.y - oldSnappedOffset.y) < 2.0) {
        snapY.skip()
      }
    }

    finishDragging(): void {
      this.stopPositionUpdate()
      this.updateNodesPosition()
    }

    createSnapGrid() {
      const nonDraggedRects = computed(() => {
        const nonDraggedNodes = iteratorFilter(
          graphStore.currentNodeIds.values(),
          (id) => !this.draggedNodes.has(id),
        )
        return Array.from(nonDraggedNodes, (id) => graphStore.nodeRects.get(id)!)
      })
      return new SnapGrid(nonDraggedRects)
    }

    updateNodesPosition() {
      for (const [id, dragged] of this.draggedNodes) {
        const node = graphStore.db.nodeIdToNode.get(id)
        if (node == null) continue
        // If node was moved in other way than current dragging, we want to stop dragging it.
        if (node.position.distanceSquared(dragged.currentPos) > 1.0) {
          this.draggedNodes.delete(id)
        } else {
          dragged.currentPos = dragged.initialPos.add(snappedOffset.value)
          graphStore.setNodePosition(id, dragged.currentPos)
        }
      }
    }
  }

  let currentDrag: CurrentDrag | undefined

  return {
    startOrUpdate(movedId: NodeId, offset: Vec2) {
      currentDrag ??= new CurrentDrag(movedId)
      currentDrag.updateOffset(offset)
    },
    finishDrag() {
      currentDrag?.finishDragging()
      currentDrag = undefined
    },
  }
}
