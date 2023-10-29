<script setup lang="ts">
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import { injectGraphSelection } from '@/providers/graphSelection.ts'
import { useGraphStore } from '@/stores/graph'
import { Interaction } from '@/util/interaction.ts'
import type { ExprId } from 'shared/yjsModel.ts'
import { watch } from 'vue'

const emit = defineEmits<{
  startInteraction: [Interaction]
  endInteraction: [Interaction]
}>()

const graphStore = useGraphStore()
const selection = injectGraphSelection(true)

class EditingEdge extends Interaction {
  cancel() {
    const target = graphStore.unconnectedEdge?.disconnectedEdgeTarget
    graphStore.transact(() => {
      if (target != null) disconnectEdge(target)
      graphStore.clearUnconnected()
    })
  }
  click(_e: MouseEvent): boolean {
    if (graphStore.unconnectedEdge == null) return false
    const source = graphStore.unconnectedEdge.source ?? selection?.hoveredNode
    const target = graphStore.unconnectedEdge.target ?? selection?.hoveredExpr
    const targetNode = target != null ? graphStore.exprNodes.get(target) : undefined
    graphStore.transact(() => {
      if (source != null && source != targetNode) {
        if (target == null) {
          if (graphStore.unconnectedEdge?.disconnectedEdgeTarget != null)
            disconnectEdge(graphStore.unconnectedEdge.disconnectedEdgeTarget)
          createNodeFromEdgeDrop(source)
        } else {
          createEdge(source, target)
        }
      }
      graphStore.clearUnconnected()
    })
    return true
  }
}
const editingEdge = new EditingEdge()

function disconnectEdge(target: ExprId) {
  graphStore.setExpressionContent(target, '_')
}
function createNodeFromEdgeDrop(source: ExprId) {
  console.log(`TODO: createNodeFromEdgeDrop(${JSON.stringify(source)})`)
}
function createEdge(source: ExprId, target: ExprId) {
  const sourceNode = graphStore.nodes.get(source)
  if (sourceNode == null) return
  // TODO: Check alias analysis to see if the binding is shadowed.
  graphStore.setExpressionContent(target, sourceNode.binding)
  // TODO: Use alias analysis to ensure declarations are in a dependency order.
}

watch(
  () => graphStore.unconnectedEdge,
  (edge) => {
    if (edge != null) {
      emit('startInteraction', editingEdge)
    } else {
      emit('endInteraction', editingEdge)
    }
  },
)
</script>

<template>
  <GraphEdge
    v-for="(edge, index) in graphStore.edges"
    :key="index"
    :edge="edge"
    :nodeRects="graphStore.nodeRects"
    :exprRects="graphStore.exprRects"
    :exprNodes="graphStore.exprNodes"
    @disconnectSource="graphStore.disconnectSource(edge)"
    @disconnectTarget="graphStore.disconnectTarget(edge)"
  />
</template>
