<script setup lang="ts">
import { injectGraphSelection } from '@/providers/graphSelection'
import { WidgetInput, type WidgetUpdate } from '@/providers/widgetRegistry'
import { WidgetEditHandlerParent } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore, type NodeId } from '@/stores/graph'
import type { NodeType } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { iconOfNode } from '@/util/getIconName'
import { computed } from 'vue'
import { DisplayIcon } from './widgets/WidgetIcon.vue'
import WidgetTreeRoot from './WidgetTreeRoot.vue'

const props = defineProps<{
  ast: Ast.Expression
  nodeId: NodeId
  rootElement: HTMLElement | undefined
  nodeType: NodeType
  potentialSelfArgumentId: Ast.AstId | undefined
  /** Ports that are not targetable by default; see {@link NodeDataFromAst}. */
  conditionalPorts: Set<Ast.AstId>
  extended: boolean
}>()
const graph = useGraphStore()
const rootPort = computed(() => {
  const input = WidgetInput.FromAst(props.ast)
  if (
    props.ast instanceof Ast.Ident &&
    (!graph.db.isKnownFunctionCall(props.ast.id) || graph.db.connections.hasValue(props.ast.id))
  ) {
    input.forcePort = true
  }

  if (!props.potentialSelfArgumentId && topLevelIcon.value) {
    input[DisplayIcon] = {
      icon: topLevelIcon.value,
      showContents: props.nodeType != 'output',
    }
  }
  return input
})
const selection = injectGraphSelection()

function selectNode() {
  selection.setSelection(new Set([props.nodeId]))
}

function handleWidgetUpdates(update: WidgetUpdate) {
  if (update.directInteraction) {
    selectNode()
  }
  const edit = update.edit ?? graph.startEdit()
  if (update.portUpdate) {
    const { origin } = update.portUpdate
    if (Ast.isAstId(origin)) {
      if ('value' in update.portUpdate) {
        const value = update.portUpdate.value
        const ast =
          value instanceof Ast.Ast ? value
          : value == null ? Ast.Wildcard.new(edit)
          : undefined
        if (ast) {
          edit.replaceValue(origin, ast)
        } else if (typeof value === 'string') {
          edit.tryGet(origin)?.syncToCode(value)
        }
      }
      if ('metadata' in update.portUpdate) {
        const { metadataKey, metadata } = update.portUpdate
        edit.tryGet(origin)?.setWidgetMetadata(metadataKey, metadata)
      }
    } else {
      console.error(`[UPDATE ${origin}] Invalid top-level origin. Expected expression ID.`)
    }
  }
  graph.commitEdit(edit)
  // This handler is guaranteed to be the last handler in the chain.
  return true
}

const topLevelIcon = computed(() => iconOfNode(props.nodeId, graph.db))

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
    :potentialSelfArgumentId="potentialSelfArgumentId"
    :input="rootPort"
    :rootElement="rootElement"
    :conditionalPorts="conditionalPorts"
    :extended="extended"
    :onUpdate="handleWidgetUpdates"
    @currentEditChanged="onCurrentEditChange"
  />
</template>
