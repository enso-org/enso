<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import UploadingFile from '@/components/GraphEditor/UploadingFile.vue'
import { useDragging } from '@/components/GraphEditor/dragging'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { useArrows, useEvent } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import type { UploadingFile as File, FileName } from '@/stores/awareness'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { AstId } from '@/util/ast/abstract'
import type { Vec2 } from '@/util/data/vec2'
import { set } from 'lib0'
import { computed, shallowRef, toRaw } from 'vue'
import { stackItemsEqual } from 'ydoc-shared/languageServerTypes'

const emit = defineEmits<{
  nodeOutputPortDoubleClick: [portId: AstId]
  nodeDoubleClick: [nodeId: NodeId]
  createNodes: [source: NodeId, options: NodeCreationOptions[]]
}>()

const projectStore = useProjectStore()
const selection = injectGraphSelection()
const graphStore = useGraphStore()
const dragging = useDragging()
const navigator = injectGraphNavigator()

function nodeIsDragged(movedId: NodeId, offset: Vec2) {
  const scaledOffset = offset.scale(1 / (navigator?.scale ?? 1))
  dragging.startOrUpdate(movedId, scaledOffset)
}

const displacingWithArrows = useArrows(
  (pos, type) => {
    const oneOfMoved = set.first(selection.selected)
    if (!oneOfMoved) return false
    dragging.startOrUpdate(oneOfMoved, pos.relative)
    if (type === 'stop') dragging.finishDrag()
    else if (type === 'cancel') dragging.cancelDrag()
  },
  { predicate: (_) => selection.selected.size > 0 },
)

useEvent(window, 'keydown', displacingWithArrows.events.keydown)

const uploadingFiles = computed<[FileName, File][]>(() => {
  const uploads = [...projectStore.awareness.allUploads()]
  if (uploads.length == 0) return []
  const currentStackItem = toRaw(projectStore.executionContext.getStackTop())
  return uploads.filter(([, file]) => stackItemsEqual(file.stackItem, currentStackItem))
})

const graphNodeSelections = shallowRef<HTMLElement>()
</script>

<template>
  <div
    ref="graphNodeSelections"
    class="layer"
    :style="{ transform: navigator.transform, 'z-index': -1 }"
  />
  <div class="layer" :style="{ transform: navigator.transform }">
    <GraphNode
      v-for="[id, node] in graphStore.db.nodeIdToNode.entries()"
      :key="id"
      :node="node"
      :edited="id === graphStore.editedNodeInfo?.id"
      :graphNodeSelections="graphNodeSelections"
      @delete="graphStore.deleteNodes([id])"
      @dragging="nodeIsDragged(id, $event)"
      @draggingCommited="dragging.finishDrag()"
      @draggingCancelled="dragging.cancelDrag()"
      @outputPortClick="(event, port) => graphStore.createEdgeFromOutput(port, event)"
      @outputPortDoubleClick="(_event, port) => emit('nodeOutputPortDoubleClick', port)"
      @doubleClick="emit('nodeDoubleClick', id)"
      @createNodes="emit('createNodes', id, $event)"
      @setNodeColor="graphStore.overrideNodeColor(id, $event)"
      @update:edited="graphStore.setEditedNode(id, $event)"
      @update:rect="graphStore.updateNodeRect(id, $event)"
      @update:hoverAnim="graphStore.updateNodeHoverAnim(id, $event)"
      @update:visualizationId="
        graphStore.setNodeVisualization(id, $event != null ? { identifier: $event } : {})
      "
      @update:visualizationRect="graphStore.updateVizRect(id, $event)"
      @update:visualizationEnabled="graphStore.setNodeVisualization(id, { visible: $event })"
      @update:visualizationWidth="graphStore.setNodeVisualization(id, { width: $event })"
      @update:visualizationHeight="graphStore.setNodeVisualization(id, { height: $event })"
    />
    <UploadingFile
      v-for="(nameAndFile, index) in uploadingFiles"
      :key="index"
      :name="nameAndFile[0]"
      :file="nameAndFile[1]"
    />
  </div>
</template>
<style scoped>
.layer {
  position: absolute;
  top: 0;
  left: 0;
  width: 0;
  height: 0;
  contain: layout size style;
  will-change: transform;
}

.layer:deep(::selection) {
  background-color: rgba(255, 255, 255, 20%);
}
</style>
