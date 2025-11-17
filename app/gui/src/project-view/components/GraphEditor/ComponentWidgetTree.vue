<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { type NodeId } from '$/providers/openedProjects/graph'
import {
  type NodeType,
  type PrimaryApplication,
} from '$/providers/openedProjects/graph/graphDatabase'
import {
  applyWidgetUpdates,
  WidgetInput,
  type WidgetUpdate,
} from '$/providers/openedProjects/widgetRegistry'
import { WidgetEditHandlerParent } from '$/providers/openedProjects/widgetRegistry/editHandler'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import WidgetTreeRoot from '@/components/GraphEditor/WidgetTreeRoot.vue'
import { useGraphSelection } from '@/providers/graphSelection'
import { Ast } from '@/util/ast'
import type { Opt } from '@/util/data/opt'
import { iconOfNode, useDisplayedIcon } from '@/util/getIconName'
import { Ok } from 'enso-common/src/utilities/data/result'
import { computed, toRef } from 'vue'

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

const { module, graph } = useCurrentProject()
const selection = useGraphSelection()

const baseIcon = computed(() => iconOfNode(props.nodeId, graph.value.db))
const { displayedIcon } = useDisplayedIcon(graph.value.db, toRef(props, 'nodeId'), baseIcon)

const rootPort = computed(() => {
  const input = WidgetInput.FromAst(props.ast)
  if (
    props.ast instanceof Ast.Ident &&
    (!graph.value.db.isKnownFunctionCall(props.ast.id) ||
      graph.value.db.connections.hasValue(props.ast.id))
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
  applyWidgetUpdates(update, module.value)
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
    :updateCallback="handleWidgetUpdates"
    @currentEditChanged="onCurrentEditChange"
  />
</template>
