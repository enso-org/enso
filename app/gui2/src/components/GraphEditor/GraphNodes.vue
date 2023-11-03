<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { SnapGrid } from '@/stores/graph/snap'
import { Rect } from '@/util/rect'
import type { Vec2 } from '@/util/vec2'
import { mapIterator } from 'lib0/iterator'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { ref } from 'vue'

const graphStore = useGraphStore()
const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)

const draggedNodes = ref<ExprId[]>([])
let snapGrid: SnapGrid | undefined

function updateNodeContent(id: ExprId, updates: [ContentRange, string][]) {
  graphStore.transact(() => {
    for (const [range, content] of updates) {
      graphStore.replaceNodeSubexpression(id, range, content)
    }
  })
}

function dragging(movedId: ExprId, offset: Vec2) {
  const scaledOffset = offset.scale(1 / (navigator?.scale ?? 1))
  if (draggedNodes.value.length === 0) {
    draggedNodes.value = selection?.isSelected(movedId) ? Array.from(selection.selected) : [movedId]
  }
  if (snapGrid == null) {
    snapGrid = graphStore.createSnapGrid(draggedNodes.value)
  }
  let rects: Rect[] = []
  for (const id of draggedNodes.value) {
    const rect = graphStore.nodeRects.get(id)
    const node = graphStore.nodes.get(id)
    if (rect != null && node != null)
      rects.push(new Rect(node.position.add(scaledOffset), rect.size))
  }
  const snap = snapGrid.snappedMany(rects, 15.0)
  console.log(snap)
  for (const id of draggedNodes.value) {
    const node = graphStore.nodes.get(id)
    if (node == null) continue
    node.visiblePosition = node.position.add(scaledOffset).add(snap)
  }
}

function draggingCommited() {
  console.log('Dragging commited')
  for (const id of draggedNodes.value) {
    const node = graphStore.nodes.get(id)
    if (node == null || node.visiblePosition == null) continue
    graphStore.setNodePosition(id, node.visiblePosition)
  }
  draggedNodes.value = []
  snapGrid = undefined
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
