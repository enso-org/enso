<script setup lang="ts">
import { useGraphStore } from '$/components/WithCurrentProject.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import WidgetTreeRoot from '@/components/GraphEditor/WidgetTreeRoot.vue'
import { injectGraphSelection } from '@/providers/graphSelection'
import { applyWidgetUpdates, WidgetInput, type WidgetUpdate } from '@/providers/widgetRegistry'
import { WidgetEditHandlerParent } from '@/providers/widgetRegistry/editHandler'
import { type NodeId } from '@/stores/graph'
import { type NodeType, type PrimaryApplication } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { Opt } from '@/util/data/opt'
import { iconOfNode, useDisplayedIcon } from '@/util/getIconName'
import { computed, toRef } from 'vue'
import { Ok } from 'ydoc-shared/util/data/result'

const props = defineProps<{
  ast: Ast.Expression
  nodeId: NodeId
  rootElement: Opt<HTMLElement>
  nodeType: NodeType
  primaryApplication: PrimaryApplication
  /** Ports that are not targetable by default; see {@link NodeDataFromAst}. */
  conditionalPorts: Set<Ast.AstId>
  extended: boolean
}>()

const graph = useGraphStore()
const selection = injectGraphSelection()

const baseIcon = computed(() => iconOfNode(props.nodeId, graph.db))
const { displayedIcon } = useDisplayedIcon(graph.db, toRef(props, 'nodeId'), baseIcon)

const rootPort = computed(() => {
  const input = WidgetInput.FromAst(props.ast)
  if (
    props.ast instanceof Ast.Ident &&
    (!graph.db.isKnownFunctionCall(props.ast.id) || graph.db.connections.hasValue(props.ast.id))
  ) {
    input.forcePort = true
  }

  if (props.primaryApplication.function == null) {
    input[DisplayIcon] = {
      icon: displayedIcon.value,
      showContents: props.nodeType != 'output',
    }
  }
  return input
})

function selectNode() {
  selection.setSelection(new Set([props.nodeId]))
}

function handleWidgetUpdates(update: WidgetUpdate) {
  if (update.directInteraction) {
    selectNode()
  }
  applyWidgetUpdates(update, graph)
  return Ok()
}

function onCurrentEditChange(currentEdit: WidgetEditHandlerParent | undefined) {
  if (currentEdit) selectNode()
}
</script>
<script lang="ts">
export const GRAB_HANDLE_X_MARGIN_L = 4
export const GRAB_HANDLE_X_MARGIN_R = 8
export const ICON_WIDTH = 16
</script>

<template>
  <WidgetTreeRoot
    class="ComponentWidgetTree"
    :externalId="nodeId"
    :primaryApplication="primaryApplication"
    :input="rootPort"
    :rootElement="rootElement"
    :conditionalPorts="conditionalPorts"
    :extended="extended"
    :onUpdate="handleWidgetUpdates"
    @currentEditChanged="onCurrentEditChange"
  />
</template>
