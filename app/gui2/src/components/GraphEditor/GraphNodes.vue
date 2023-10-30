<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import type { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId } from 'shared/yjsModel'

const graphStore = useGraphStore()
const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)

function updateNodeContent(id: ExprId, updates: [ContentRange, string][]) {
  graphStore.transact(() => {
    for (const [range, content] of updates) {
      graphStore.replaceNodeSubexpression(id, range, content)
    }
  })
}

function moveNode(movedId: ExprId, delta: Vec2) {
  const scaledDelta = delta.scale(1 / (navigator?.scale ?? 1))
  graphStore.transact(() => {
    for (const id of selection?.isSelected(movedId) ? selection.selected : [movedId]) {
      const node = graphStore.nodes.get(id)
      if (node == null) continue
      graphStore.setNodePosition(id, node.position.add(scaledDelta))
    }
  })
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
    @movePosition="moveNode(id, $event)"
    @outputPortAction="graphStore.createEdgeFromOutput(id)"
  />
</template>
