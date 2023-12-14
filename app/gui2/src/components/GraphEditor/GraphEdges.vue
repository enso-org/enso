<script setup lang="ts">
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { useGraphStore } from '@/stores/graph'
import { Vec2 } from '@/util/data/vec2'
import type { ExprId } from 'shared/yjsModel'

const graph = useGraphStore()
const selection = injectGraphSelection(true)
const interaction = injectInteractionHandler()

const emits = defineEmits<{
  createNodeFromEdge: [source: ExprId, position: Vec2]
}>()

const editingEdge: Interaction = {
  cancel() {
    const target = graph.unconnectedEdge?.disconnectedEdgeTarget
    graph.transact(() => {
      if (target != null) disconnectEdge(target)
      graph.clearUnconnected()
    })
  },
  click(_e: MouseEvent, graphNavigator: GraphNavigator): boolean {
    if (graph.unconnectedEdge == null) return false
    const source = graph.unconnectedEdge.source ?? selection?.hoveredNode
    const target = graph.unconnectedEdge.target ?? selection?.hoveredPort
    const targetNode = target && graph.db.getExpressionNodeId(target)
    graph.transact(() => {
      if (source != null && source != targetNode) {
        if (target == null) {
          if (graph.unconnectedEdge?.disconnectedEdgeTarget != null)
            disconnectEdge(graph.unconnectedEdge.disconnectedEdgeTarget)
          emits('createNodeFromEdge', source, graphNavigator.sceneMousePos ?? Vec2.Zero)
        } else {
          createEdge(source, target)
        }
      }
      graph.clearUnconnected()
    })
    return true
  },
}
interaction.setWhen(() => graph.unconnectedEdge != null, editingEdge)

function disconnectEdge(target: ExprId) {
  graph.setExpressionContent(target, '_')
}

function createEdge(source: ExprId, target: ExprId) {
  const ident = graph.db.getOutputPortIdentifier(source)
  if (ident == null) return
  // TODO: Check alias analysis to see if the binding is shadowed.
  graph.setExpressionContent(target, ident)
  // TODO: Use alias analysis to ensure declarations are in a dependency order.
}
</script>

<template>
  <GraphEdge v-for="(edge, index) in graph.edges" :key="index" :edge="edge" />
</template>
