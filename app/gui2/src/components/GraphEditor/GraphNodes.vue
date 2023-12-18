<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import UploadingFile from '@/components/GraphEditor/UploadingFile.vue'
import { useDragging } from '@/components/GraphEditor/dragging'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import type { UploadingFile as File, FileName } from '@/stores/awareness'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { Vec2 } from '@/util/data/vec2'
import { stackItemsEqual } from 'shared/languageServerTypes'
import type { ExprId } from 'shared/yjsModel'
import { computed, toRaw } from 'vue'

const projectStore = useProjectStore()
const graphStore = useGraphStore()
const dragging = useDragging()
const selection = injectGraphSelection(true)
const navigator = injectGraphNavigator(true)

const emit = defineEmits<{
  nodeOutputPortDoubleClick: [portId: ExprId]
  nodeDoubleClick: [nodeId: ExprId]
}>()

function nodeIsDragged(movedId: ExprId, offset: Vec2) {
  const scaledOffset = offset.scale(1 / (navigator?.scale ?? 1))
  dragging.startOrUpdate(movedId, scaledOffset)
}

function hoverNode(id: ExprId | undefined) {
  if (selection != null) selection.hoveredNode = id
}

const uploadingFiles = computed<[FileName, File][]>(() => {
  const currentStackItem = projectStore.executionContext.getStackTop()
  return [...projectStore.awareness.allUploads()].filter(([, file]) =>
    stackItemsEqual(file.stackItem, toRaw(currentStackItem)),
  )
})
</script>

<template>
  <GraphNode
    v-for="[id, node] in graphStore.db.nodeIdToNode.entries()"
    :key="id"
    :node="node"
    :edited="id === graphStore.editedNodeInfo?.id"
    @pointerenter="hoverNode(id)"
    @pointerleave="hoverNode(undefined)"
    @dragging="nodeIsDragged(id, $event)"
    @draggingCommited="dragging.finishDrag()"
    @outputPortClick="graphStore.createEdgeFromOutput($event)"
    @outputPortDoubleClick="emit('nodeOutputPortDoubleClick', $event)"
    @doubleClick="emit('nodeDoubleClick', id)"
    @update:edited="graphStore.setEditedNode(id, $event)"
    @update:rect="graphStore.updateNodeRect(id, $event)"
    @update:visualizationId="graphStore.setNodeVisualizationId(id, $event)"
    @update:visualizationRect="graphStore.updateVizRect(id, $event)"
    @update:visualizationVisible="graphStore.setNodeVisualizationVisible(id, $event)"
  />
  <UploadingFile
    v-for="(nameAndFile, index) in uploadingFiles"
    :key="index"
    :name="nameAndFile[0]"
    :file="nameAndFile[1]"
  />
</template>
