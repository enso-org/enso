<script setup lang="ts">
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Vec2 } from '@/util/data/vec2'
import { toast } from 'react-toastify'
import { type ExprId } from 'shared/yjsModel'

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
    const targetNode = target instanceof Ast.Ast && graph.getPortNodeId(target)
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
  graph.editScope((edit) => {
    if (!graph.updatePortValue(edit, target, undefined)) {
      if (target instanceof Ast.Ast) {
        console.warn(`Failed to disconnect edge from target, falling back to direct edit.`, target)
        const target_ = edit.get(target)
        if (target_) {
          target_.take()
        } else {
          console.error(`Cannot disconnect target: not found in module.`, target)
        }
      } else {
        console.error(`Failed to disconnect edge from target, no fallback possible.`, target)
      }
    }
  })
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

  if (!graph.astModule) return
  const edit = graph.astModule.edit()
  const reorderResult = graph.ensureCorrectNodeOrder(edit, sourceNode, targetNode)
  if (reorderResult === 'circular') {
    // Creating this edge would create a circular dependency. Prevent that and display error.
    toast.error('Could not connect due to circular dependency.')
  } else {
    if (!graph.updatePortValue(edit, target, identAst)) {
      if (target instanceof Ast.Ast) {
        console.warn(`Failed to connect edge to target port, falling back to direct edit.`, target)
        const target_ = edit.get(target)
        if (target_) {
          target_.replaceValue(identAst)
          graph.commitEdit(edit)
        } else {
          console.error(`Cannot disconnect target: not found in module.`, target)
        }
      } else {
        console.error(`Failed to connect edge to target port, no fallback possible.`, target)
      }
    }
  }
}
</script>

<template>
  <GraphEdge v-for="(edge, index) in graph.edges" :key="index" :edge="edge" />
</template>
