<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { Drag } from './dragging'

const graphStore = useGraphStore()
const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)

let drag: Drag | undefined

function updateNodeContent(id: ExprId, updates: [ContentRange, string][]) {
  graphStore.transact(() => {
    for (const [range, content] of updates) {
      graphStore.replaceNodeSubexpression(id, range, content)
    }
  })
}

function dragging(movedId: ExprId, offset: Vec2) {
  drag ??= new Drag(movedId, selection)
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
