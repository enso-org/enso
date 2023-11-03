<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { SnapGrid } from '@/components/GraphEditor/snap'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { useApproach } from '@/util/animation'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { iteratorFilter } from 'lib0/iterator'
import { abs } from 'lib0/math'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { ref, watchEffect, type Ref, type WatchStopHandle } from 'vue'

const graphStore = useGraphStore()
const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)

class Drag {
  static THRESHOLD = 15.0

  draggedNodes: ExprId[]
  grid: SnapGrid
  offset = ref(Vec2.Zero)
  snapXTarget = ref(0.0)
  snapX = useApproach(this.snapXTarget, 30.0)
  snapYTarget: Ref<number> = ref(0.0)
  snapY = useApproach(this.snapYTarget, 30.0)
  stopPositionUpdate: WatchStopHandle

  constructor(movedId: ExprId) {
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
    let rects: Rect[] = []
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
      delete node.visiblePosition
    }
  }

  private createSnapGrid() {
    const excludeSet = new Set<ExprId>()
    for (const node of this.draggedNodes) excludeSet.add(node)
    const withoutExcluded = iteratorFilter(
      graphStore.nodeRects.entries(),
      ([id]) => !excludeSet.has(id),
    )
    return new SnapGrid(Array.from(withoutExcluded, ([_, rect]) => rect))
  }
}

let drag: Drag | undefined

function updateNodeContent(id: ExprId, updates: [ContentRange, string][]) {
  graphStore.transact(() => {
    for (const [range, content] of updates) {
      graphStore.replaceNodeSubexpression(id, range, content)
    }
  })
}

function dragging(movedId: ExprId, offset: Vec2) {
  if (drag == null) {
    drag = new Drag(movedId)
  }
  const scaledOffset = offset.scale(1 / (navigator?.scale ?? 1))
  drag.updateOffset(scaledOffset)
}

function draggingCommited() {
  console.log('Dragging commited')
  drag?.finishDragging()
  drag = undefined
}

function hoverNode(id: ExprId | undefined) {
  if (selection != null) selection.hoveredNode = id
}
</script>

<template>
  <GraphNode
    v-for="[id, node] in graphStore.nodes"
    :key="id"
    :node="node"
    @updateRect="graphStore.updateNodeRect(id, $event)"
    @delete="graphStore.deleteNode(id)"
    @updateExprRect="graphStore.updateExprRect"
    @pointerenter="hoverNode(id)"
    @pointerleave="hoverNode(undefined)"
    @updateContent="updateNodeContent(id, $event)"
    @setVisualizationId="graphStore.setNodeVisualizationId(id, $event)"
    @setVisualizationVisible="graphStore.setNodeVisualizationVisible(id, $event)"
    @dragging="dragging(id, $event)"
    @draggingCommited="draggingCommited"
    @outputPortAction="graphStore.createEdgeFromOutput(id)"
  />
</template>
