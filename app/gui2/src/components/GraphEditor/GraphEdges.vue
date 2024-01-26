<script setup lang="ts">
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract.ts'
import { Vec2 } from '@/util/data/vec2'
import { toast } from 'react-toastify'
import { isUuid } from 'shared/yjsModel.ts'

const graph = useGraphStore()
const selection = injectGraphSelection(true)
const interaction = injectInteractionHandler()

const emits = defineEmits<{
  createNodeFromEdge: [source: AstId, position: Vec2]
}>()

const editingEdge: Interaction = {
  cancel() {
    graph.clearUnconnected()
  },
  click(_e: MouseEvent, graphNavigator: GraphNavigator): boolean {
    if (graph.unconnectedEdge == null) return false
    let source: AstId | undefined
    let sourceNode: NodeId | undefined
    if (graph.unconnectedEdge.source) {
      source = graph.unconnectedEdge.source
      sourceNode = graph.db.getPatternExpressionNodeId(source)
    } else if (selection?.hoveredNode) {
      sourceNode = selection.hoveredNode
      source = graph.db.getNodeFirstOutputPort(sourceNode)
    }
    const target = graph.unconnectedEdge.target ?? selection?.hoveredPort
    const targetNode = target && graph.getPortNodeId(target)
    console.log(source, target, targetNode)
    graph.transact(() => {
      if (source != null && sourceNode != targetNode) {
        if (target == null) {
          if (graph.unconnectedEdge?.disconnectedEdgeTarget != null)
            disconnectEdge(graph.unconnectedEdge.disconnectedEdgeTarget)
          emits('createNodeFromEdge', source, graphNavigator.sceneMousePos ?? Vec2.Zero)
        } else {
          createEdge(source, target)
        }
      } else if (source == null && target != null) {
        disconnectEdge(target)
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
      const targetStr: string = target
      if (isUuid(targetStr)) {
        console.warn(`Failed to disconnect edge from port ${target}, falling back to direct edit.`)
        edit.replaceRef(targetStr as AstId, Ast.Wildcard.new())
      } else {
        console.error(`Failed to disconnect edge from port ${target}, no fallback possible.`)
      }
    }
  })
}

function createEdge(source: AstId, target: PortId) {
  const ident = graph.db.getOutputPortIdentifier(source)
  if (ident == null) return
  const identAst = Ast.parse(ident)

  const sourceNode = graph.db.getPatternExpressionNodeId(source)
  const targetNode = graph.getPortNodeId(target)
  if (sourceNode == null || targetNode == null) {
    console.log(sourceNode, targetNode, source, target)
    return console.error(`Failed to connect edge, source or target node not found.`)
  }

  const edit = graph.astModule.edit()
  const reorderResult = graph.ensureCorrectNodeOrder(edit, sourceNode, targetNode)
  if (reorderResult === 'circular') {
    // Creating this edge would create a circular dependency. Prevent that and display error.
    toast.error('Could not connect due to circular dependency.')
  } else {
    if (!graph.updatePortValue(edit, target, identAst)) {
      if (isUuid(target)) {
        console.warn(`Failed to connect edge to port ${target}, falling back to direct edit.`)
        edit.replaceValue(Ast.asAstId(target), identAst)
        graph.commitEdit(edit)
      } else {
        console.error(`Failed to connect edge to port ${target}, no fallback possible.`)
      }
    }
  }
}
</script>

<template>
  <GraphEdge v-for="(edge, index) in graph.edges" :key="index" :edge="edge" />
</template>
