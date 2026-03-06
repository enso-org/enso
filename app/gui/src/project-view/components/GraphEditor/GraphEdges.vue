<script lang="ts">
export const EDGE_ARROW_MARKER_ID = 'edge-arrow'
export const EDGE_ARROW_PATH =
  'M10.9635 1.5547 L6.83205 7.75193 C6.43623 8.34566 5.56377 8.34566 5.16795 7.75192 L1.03647 1.5547 C0.593431 0.890146 1.06982 0 1.86852 0 L10.1315 0 C10.9302 0 11.4066 0.890147 10.9635 1.5547 Z'
</script>
<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { type NodeId } from '$/providers/openedProjects/graph'
import { requiredImports } from '$/providers/openedProjects/module/imports'
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import GraphNodeOutputPorts from '@/components/GraphEditor/GraphNodeOutputPorts.vue'
import { useEventConditional } from '@/composables/events'
import { type GraphNavigator } from '@/providers/graphNavigator'
import { useGraphSelection } from '@/providers/graphSelection'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import { Ast } from '@/util/ast'
import { isAstId, type AstId } from '@/util/ast/abstract'
import { Vec2 } from '@/util/data/vec2'
import { ProjectPath } from '@/util/projectPath'
import { useToast } from '@/util/toast'
import { Err, Ok, unwrapOr, unwrapOrWithLog } from 'enso-common/src/utilities/data/result'
import { computed } from 'vue'
import ContextMenuTrigger from '../ContextMenuTrigger.vue'

const {
  projectNames: projectNames,
  store: project,
  module,
  graph,
  suggestionDb,
} = useCurrentProject()
const selection = useGraphSelection(true)
const interaction = injectInteractionHandler()
const connectionToast = useToast.error()

const props = defineProps<{
  navigator: GraphNavigator
}>()

const emit = defineEmits<{
  createNodeFromEdge: [source: AstId, position: Vec2]
  createNodeFromPort: [portId: AstId]
}>()

const MIN_DRAG_MOVE = 10

const editingEdge: Interaction = {
  cancel: () => (graph.value.mouseEditedEdge = undefined),
  end: () => (graph.value.mouseEditedEdge = undefined),
  pointerdown: (e: PointerEvent) => {
    if (edgeInteractionClick()) {
      e.preventDefault()
      e.stopPropagation()
    }
  },
}

useEventConditional(
  window,
  'pointerup',
  () => interaction.getCurrent() === editingEdge,
  (e: PointerEvent) => {
    const editedEdge = graph.value.mouseEditedEdge
    if (editedEdge == null) return
    const delta = new Vec2(e.screenX, e.screenY).sub(
      new Vec2(editedEdge.startPosition.x, editedEdge.startPosition.y),
    )
    if (delta.lengthSquared() >= MIN_DRAG_MOVE ** 2) {
      if (edgeInteractionClick()) e.stopPropagation()
    } else if (editedEdge.createdFrom === 'newNodeButton' && editedEdge.source != null) {
      // The distance of a drag is not important when dragging from the new node button.
      // The button will not initiate editedEdge if the drag is too short.
      createNewNodeFromPort(editedEdge.source)
    }
  },
  { capture: true },
)

function edgeInteractionClick() {
  if (graph.value.mouseEditedEdge == null) return false
  let source: AstId | undefined
  let sourceNode: NodeId | undefined
  if (graph.value.mouseEditedEdge.source) {
    source = graph.value.mouseEditedEdge.source
    sourceNode = graph.value.db.getPatternExpressionNodeId(source)
  } else if (selection?.hoveredNode) {
    sourceNode = selection.hoveredNode
    source = graph.value.db.getNodeFirstOutputPort(sourceNode)
  }
  const target = graph.value.mouseEditedEdge.target ?? selection?.hoveredPort
  const targetNode = target && graph.value.getPortNodeId(target)
  if (source != null && sourceNode != targetNode) {
    if (target == null) {
      if (graph.value.mouseEditedEdge?.disconnectedEdgeTarget != null)
        disconnectEdge(graph.value.mouseEditedEdge.disconnectedEdgeTarget)
      emit('createNodeFromEdge', source, props.navigator.sceneMousePos ?? Vec2.Zero)
    } else {
      createEdge(source, target)
    }
  } else if (source == null && target != null) {
    disconnectEdge(target)
  }
  graph.value.mouseEditedEdge = undefined
  return true
}

interaction.setWhen(() => graph.value.mouseEditedEdge != null, editingEdge)

async function disconnectEdge(target: PortId) {
  const result = await module.value.edit(async (edit) => {
    const updateResult = await graph.value.updatePortValue(target, undefined, edit, false)
    if (!updateResult.ok) {
      if (isAstId(target)) {
        console.warn(`Failed to disconnect edge from port ${target}, falling back to direct edit.`)
        edit.replaceValue(target, Ast.Wildcard.new(edit))
        return Ok()
      }
    }
    return updateResult
  })
  if (!result.ok) result.error.log(`Failed to disconnect edge from port ${target}`)
}

async function createEdge(source: AstId, target: PortId) {
  const graph_ = graph.value
  const ident = graph_.db.getOutputPortIdentifier(source)
  if (ident == null) return

  const sourceNode = graph_.getSourceNodeId(source)
  const targetNode = graph_.getPortNodeId(target)
  if (sourceNode == null || targetNode == null) {
    return console.error(`Failed to connect edge, source or target node not found.`)
  }

  const result = await module.value.edit(async (edit) => {
    const reorderResult = graph_.ensureCorrectNodeOrder(edit, sourceNode, targetNode)
    if (reorderResult === 'circular') {
      // Creating this edge would create a circular dependency. Prevent that and display error.
      const err = Err('Could not connect due to circular dependency')
      connectionToast.show(err.error.payload)
      return err
    } else {
      const identAst = Ast.parseExpression(ident, edit)!
      const expectedType = unwrapOr(
        projectNames.value.parseProjectPathRaw(graph_.getPortExpectedType(target) ?? ''),
        undefined,
      )
      const connectionType =
        project.value.computedValueRegistry.getExpressionInfo(sourceNode)?.typeInfo
      // Check if type cast to the target type is both possible and necessary.
      const findCompatibleType = (
        list: ProjectPath[] | undefined,
        withType: ProjectPath | undefined,
      ) => {
        return list
          ?.flatMap((type) =>
            unwrapOrWithLog(suggestionDb.value.entries.getTypeAndItsParentsEntries(type), []),
          )
          .find((type) => withType?.equals(type.definitionPath))
      }
      const castNeeded = findCompatibleType(connectionType?.visibleTypes, expectedType) == null
      const targetType = castNeeded && findCompatibleType(connectionType?.hiddenTypes, expectedType)
      let portValueToSet = undefined
      if (targetType) {
        module.value.addMissingImports(
          edit,
          requiredImports(suggestionDb.value.entries, targetType),
        )
        if (!Ast.isIdentifier(targetType.name)) {
          console.error(
            'SuggestionDB has a type which is not an identifier:',
            targetType.definitionPath,
          )
        } else {
          portValueToSet = Ast.TypeAnnotated.new(
            edit,
            identAst,
            Ast.Ident.new(edit, targetType.name),
          )
        }
      }
      portValueToSet = portValueToSet ?? identAst

      const updateResult = await graph_.updatePortValue(target, portValueToSet, edit)
      if (!updateResult.ok) {
        if (isAstId(target)) {
          console.warn(`Failed to connect edge to port ${target}, falling back to direct edit.`)
          edit.replaceValue(target, portValueToSet)
          return Ok()
        }
      }
      return updateResult
    }
  })
  if (!result.ok) result.error.log(`Failed to connect edge to port ${target}`)
}

const nodeIdsWithOutputPorts = computed(() =>
  [...graph.value.db.nodeOutputPorts.allForward()].map(([id]) => id),
)

function createNewNodeFromPort(id: AstId) {
  const nodeId = graph.value.getOutputPortNodeId(id)
  if (nodeId != null) {
    selection?.setSelection(new Set([nodeId]))
  }
  emit('createNodeFromPort', id)
}
</script>
<template>
  <div>
    <ContextMenuTrigger :actions="['graph.deleteSelectedEdge']">
      <svg :viewBox="props.navigator.viewBox" class="overlay behindNodes">
        <marker
          :id="EDGE_ARROW_MARKER_ID"
          viewBox="0 0 12 9"
          refX="6"
          refY="1"
          markerWidth="12"
          markerHeight="9"
          markerUnits="userSpaceOnUse"
          orient="0"
          fill="context-stroke"
        >
          <path :d="EDGE_ARROW_PATH" />
        </marker>

        <GraphEdge v-for="edge in graph.connectedEdges" :key="edge.target" :edge="edge" />
        <GraphEdge v-if="graph.cbEditedEdge" :edge="graph.cbEditedEdge" />
        <GraphEdge
          v-if="graph.outputSuggestedEdge"
          :edge="graph.outputSuggestedEdge"
          animateFromSourceHover
        />
        <GraphEdge
          v-for="edge in graph.createNodeFromOutputPortButtonEdges"
          :key="edge.source"
          :edge="edge"
          :arrow="false"
          animateFromSourceHover
        />
        <template v-for="id in nodeIdsWithOutputPorts" :key="id">
          <GraphNodeOutputPorts
            v-show="id !== graph.editedNodeInfo?.id"
            :nodeId="id"
            @newNodeClick="(portId) => createNewNodeFromPort(portId)"
            @newNodeDrag="(portId) => graph.createEdgeFromNewButton(portId)"
            @portClick="(event, portId) => graph.createEdgeFromOutput(portId, event)"
            @portDoubleClick="(_event, portId) => emit('createNodeFromPort', portId)"
          />
        </template>
      </svg>
    </ContextMenuTrigger>
    <svg v-if="graph.mouseEditedEdge" :viewBox="props.navigator.viewBox" class="overlay aboveNodes">
      <GraphEdge data-testid="mouse-edited-edge" :edge="graph.mouseEditedEdge" maskSource />
    </svg>
  </div>
</template>

<style scoped>
.overlay {
  position: absolute;
  top: 0;
  left: 0;
  pointer-events: none;
}

.overlay.behindNodes {
  z-index: -1;
}

.overlay.aboveNodes {
  z-index: 20;
}
</style>
