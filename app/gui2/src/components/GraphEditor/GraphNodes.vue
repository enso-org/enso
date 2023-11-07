<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { SnapGrid, useDragging } from '@/components/GraphEditor/snapGrid'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { useApproach } from '@/util/animation'
import { Rect } from '@/util/rect'
import type { SelectionComposable } from '@/util/selection'
import { Vec2 } from '@/util/vec2'
import { iteratorFilter } from 'lib0/iterator'
import { abs } from 'lib0/math'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { ref, watchEffect, type WatchStopHandle } from 'vue'

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
  dragging.update(movedId, scaledOffset)
}

function hoverNode(id: ExprId | undefined) {
  if (selection != null) selection.hoveredNode = id
}
</script>

<template>
  <GraphNode
    v-for="[id, node] in graphStore.nodes"
    v-show="id != graphStore.editedNodeInfo?.id"
    :key="id"
    :node="node"
    :edited="false"
    @update:edited="graphStore.setEditedNode(id, $event)"
    @updateRect="graphStore.updateNodeRect(id, $event)"
    @delete="graphStore.deleteNode(id)"
    @updateExprRect="graphStore.updateExprRect"
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
