<script setup lang="ts">
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import { useGraphStore } from '@/stores/graph'
import { assertNever } from '@/util/assert'
import { Ast } from '@/util/ast'
import { Vec2 } from '@/util/data/vec2'
import { toast } from 'react-toastify'
import { isUuid, type ExprId } from 'shared/yjsModel.ts'
import { nextTick } from 'vue'

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
    const targetNode = target && graph.getPortNodeId(target)
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

function disconnectEdge(target: PortId) {
  if (!graph.updatePortValue(target, undefined)) {
    const targetStr: string = target
    if (isUuid(targetStr)) {
      console.warn(`Failed to disconnect edge from port ${target}, falling back to direct edit.`)
      graph.setExpressionContent(targetStr as ExprId, '_')
    } else {
      console.error(`Failed to disconnect edge from port ${target}, no fallback possible.`)
    }
  }
}

function createEdge(source: ExprId, target: PortId) {
  const ident = graph.db.getOutputPortIdentifier(source)
  if (ident == null) return
  const identAst = Ast.parse(ident)

  const sourceNode = graph.db.getPatternExpressionNodeId(source)
  const targetNode = graph.getPortNodeId(target)
  if (sourceNode == null || targetNode == null) {
    console.log(sourceNode, targetNode, source, target)
    return console.error(`Failed to connect edge, source or target node not found.`)
  }

  const makeConnectionEdit = () => {
    if (!graph.updatePortValue(target, identAst)) {
      if (isUuid(target)) {
        console.warn(`Failed to connect edge to port ${target}, falling back to direct edit.`)
        graph.setExpressionContent(target, ident)
      } else {
        console.error(`Failed to connect edge to port ${target}, no fallback possible.`)
      }
    }
  }

  const reorderResult = graph.ensureCorrectNodeOrder(sourceNode, targetNode)
  if (reorderResult === true) {
    // To allow node reordering edit to complete, delay edge insertion update to the next tick.
    nextTick(makeConnectionEdit)
  } else if (reorderResult === false) {
    // Reordering wasn't necessary, perform the edit immediately.
    makeConnectionEdit()
  } else if (reorderResult === 'circular') {
    // Creating this edge would create a circular dependency. Prevent that and display error.
    toast.error('Could not connect due to circular dependency.')
  } else {
    assertNever(reorderResult)
  }
}
</script>

<template>
  <GraphEdge v-for="(edge, index) in graph.edges" :key="index" :edge="edge" />
</template>
