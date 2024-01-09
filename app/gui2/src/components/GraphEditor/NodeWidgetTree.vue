<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { useTransitioning } from '@/composables/animation'
import { WidgetInput, type UpdatePayload } from '@/providers/widgetRegistry'
import { provideWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { assertNever } from '@/util/assert'
import { Ast } from '@/util/ast'
import { isUuid } from 'shared/yjsModel'
import { computed, toRef } from 'vue'

const props = defineProps<{ ast: Ast.Ast }>()
const graph = useGraphStore()
const rootPort = computed(() => {
  const input = WidgetInput.FromAst(props.ast)
  if (props.ast instanceof Ast.Ident && !graph.db.isKnownFunctionCall(props.ast.exprId)) {
    input.forcePort = true
  }
  return input
})

const observedLayoutTransitions = new Set([
  'margin-left',
  'margin-right',
  'margin-top',
  'margin-bottom',
  'padding-left',
  'padding-right',
  'padding-top',
  'padding-bottom',
  'width',
  'height',
])

function handleWidgetUpdates(update: UpdatePayload) {
  console.log('Widget Update: ', update)
  if (update.type === 'edit') {
    graph.commitEdit(update.edit)
  } else if (update.type === 'set') {
    const { value, origin } = update
    if (!isUuid(origin)) {
      console.error(`[UPDATE ${origin}] Invalid top-level origin. Expected expression ID.`)
    } else if (value instanceof Ast.Ast) {
      const edit = graph.astModule.edit()
      edit.replaceValue(origin as Ast.AstId, value)
      graph.commitEdit(edit)
    } else if (typeof value === 'string') {
      graph.setExpressionContent(origin, value)
    } else if (value == null) {
      graph.setExpressionContent(origin, '_')
    } else {
      console.error(`[UPDATE ${origin}] Invalid value:`, value)
    }
  } else {
    assertNever(update)
  }

  // No matter if it's a success or not, this handler is always considered to have handled the update,
  // since it is guaranteed to be the last handler in the chain.
  return true
}

const layoutTransitions = useTransitioning(observedLayoutTransitions)
provideWidgetTree(toRef(props, 'ast'), layoutTransitions.active)
</script>

<template>
  <div class="NodeWidgetTree" spellcheck="false" v-on="layoutTransitions.events">
    <NodeWidget :input="rootPort" @update="handleWidgetUpdates" />
  </div>
</template>

<style scoped>
.NodeWidgetTree {
  color: white;
  margin-left: 4px;

  outline: none;
  height: 24px;
  display: flex;
  align-items: center;

  &:has(.WidgetPort.newToConnect) {
    margin-left: calc(4px - var(--widget-port-extra-pad));
  }

  &:has(.WidgetPort.newToConnect > .r-24:only-child) {
    margin-left: 4px;
  }
}

.GraphEditor.draggingEdge .NodeWidgetTree {
  transition: margin 0.2s ease;
}
</style>
