<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { useDragging } from '@/components/GraphEditor/dragging'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import type { Rect } from '@/util/rect'
import type { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId } from 'shared/yjsModel'

const graphStore = useGraphStore()
const dragging = useDragging()
const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)

function updateNodeContent(id: ExprId, updates: [ContentRange, string][]) {
  graphStore.transact(() => {
    for (const [range, content] of updates) {
      graphStore.replaceNodeSubexpression(id, range, content)
    }
  })
}

function nodeIsDragged(movedId: ExprId, offset: Vec2) {
  const scaledOffset = offset.scale(1 / (navigator?.scale ?? 1))
  dragging.startOrUpdate(movedId, scaledOffset)
}

function hoverNode(id: ExprId | undefined) {
  if (selection != null) selection.hoveredNode = id
}

function updateExprRect(nodeId: ExprId, expr: ExprId, rect: Rect) {
  const nodeRect = graphStore.nodeRects.get(nodeId)
  if (nodeRect == null) return
  const localRect = rect.offsetBy(nodeRect.pos.inverse())
  graphStore.updateExprRect(expr, localRect)
}
</script>

<template>
  <GraphNode
    v-for="[id, node] in graphStore.db.allNodes()"
    v-show="id != graphStore.editedNodeInfo?.id"
    :key="id"
    :node="node"
    :edited="false"
    @update:edited="graphStore.setEditedNode(id, $event)"
    @updateRect="graphStore.updateNodeRect(id, $event)"
    @delete="graphStore.deleteNode(id)"
    @updateExprRect="(expr, rect) => updateExprRect(id, expr, rect)"
    @pointerenter="hoverNode(id)"
    @pointerleave="hoverNode(undefined)"
    @updateContent="updateNodeContent(id, $event)"
    @setVisualizationId="graphStore.setNodeVisualizationId(id, $event)"
    @setVisualizationVisible="graphStore.setNodeVisualizationVisible(id, $event)"
    @dragging="nodeIsDragged(id, $event)"
    @draggingCommited="dragging.finishDrag()"
    @outputPortAction="graphStore.createEdgeFromOutput(id)"
  />
</template>
