<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import UploadingFile from '@/components/GraphEditor/UploadingFile.vue'
import { useDragging } from '@/components/GraphEditor/dragging'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import type { UploadingFile as File, FileName } from '@/stores/awareness'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { AstId } from '@/util/ast/abstract'
import type { Vec2 } from '@/util/data/vec2'
import { stackItemsEqual } from 'shared/languageServerTypes'
import { computed, toRaw } from 'vue'

const props = defineProps<{
  graphNodeSelections: HTMLElement | undefined
}>()

const emit = defineEmits<{
  nodeOutputPortDoubleClick: [portId: AstId]
  nodeDoubleClick: [nodeId: NodeId]
  createNodes: [source: NodeId, options: NodeCreationOptions[]]
  setNodeColor: [color: string]
}>()

const projectStore = useProjectStore()
const graphStore = useGraphStore()
const dragging = useDragging()
const navigator = injectGraphNavigator(true)

function nodeIsDragged(movedId: NodeId, offset: Vec2) {
  const scaledOffset = offset.scale(1 / (navigator?.scale ?? 1))
  dragging.startOrUpdate(movedId, scaledOffset)
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
    :graphNodeSelections="props.graphNodeSelections"
    :data-node="id"
    @delete="graphStore.deleteNodes([id])"
    @dragging="nodeIsDragged(id, $event)"
    @draggingCommited="dragging.finishDrag()"
    @outputPortClick="(event, port) => graphStore.createEdgeFromOutput(port, event)"
    @outputPortDoubleClick="(_event, port) => emit('nodeOutputPortDoubleClick', port)"
    @doubleClick="emit('nodeDoubleClick', id)"
    @createNodes="emit('createNodes', id, $event)"
    @setNodeColor="emit('setNodeColor', $event)"
    @update:edited="graphStore.setEditedNode(id, $event)"
    @update:rect="graphStore.updateNodeRect(id, $event)"
    @update:visualizationId="
      graphStore.setNodeVisualization(id, $event != null ? { identifier: $event } : {})
    "
    @update:visualizationRect="graphStore.updateVizRect(id, $event)"
    @update:visualizationVisible="graphStore.setNodeVisualization(id, { visible: $event })"
    @update:visualizationFullscreen="graphStore.setNodeVisualization(id, { fullscreen: $event })"
    @update:visualizationWidth="graphStore.setNodeVisualization(id, { width: $event })"
    @update:visualizationHeight="graphStore.setNodeVisualization(id, { height: $event })"
  />
  <UploadingFile
    v-for="(nameAndFile, index) in uploadingFiles"
    :key="index"
    :name="nameAndFile[0]"
    :file="nameAndFile[1]"
  />
</template>
