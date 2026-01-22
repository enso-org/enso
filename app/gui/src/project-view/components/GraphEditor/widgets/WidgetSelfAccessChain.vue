<script setup lang="ts">
import { useGraphStore } from '$/components/WithCurrentProject.vue'
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import { injectFunctionInfo } from '@/providers/functionInfo'
import { injectKeyboard } from '@/providers/keyboard'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { unwrapGroups } from '@/util/ast/abstract'
import { displayedIconOf, useDisplayedIcon } from '@/util/getIconName'
import { computed, toRef } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const functionInfo = injectFunctionInfo(true)
const graph = useGraphStore()
const tree = injectWidgetTree()

const baseIcon = computed(() => {
  const callInfo = functionInfo?.callInfo
  return displayedIconOf(
    callInfo?.suggestion,
    callInfo?.methodCall.methodPointer,
    functionInfo?.outputType,
  )
})
const { displayedIcon } = useDisplayedIcon(graph.db, toRef(tree, 'externalId'), baseIcon)

const iconInput = computed(() => {
  const lhs = props.input.value.lhs
  if (!lhs) return
  const expression = unwrapGroups(lhs)
  let portId = expression.id
  if (expression instanceof Ast.TypeAnnotated) {
    // The PortId of the type annotated expression is the internal expression,
    // not the whole Ast.TypeAnnotated, so that the connections are displayed correctly.
    const innerExpr = unwrapGroups(expression.expression)
    portId = innerExpr.id
  }
  const input = WidgetInput.WithPort(WidgetInput.FromAstWithPortId(expression, portId))
  const isTypeAnnotated = expression instanceof Ast.TypeAnnotated
  input[DisplayIcon] = {
    icon: displayedIcon.value,
    showContents: isTypeAnnotated,
    noGap: isTypeAnnotated,
  }
  return input
})

const keyboard = injectKeyboard(true)
const extendedPortId = computed(() => {
  // Do not interfere with more detailed port picking.
  if (keyboard?.mod) return undefined
  return iconInput.value?.portId
})

const rootProps = computed(() => {
  const props: Record<string, string> = {}
  if (extendedPortId.value) props['data-port'] = extendedPortId.value
  return props
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.PropertyAccess),
  {
    priority: 999,
    score: (info) => {
      const tree = injectWidgetTree()
      const selfId = tree.primaryApplication.function
      const subject = info.input.value
      if (subject.id === selfId) {
        return Score.Good
      }
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelfAccessChain widgetParent" v-bind="rootProps">
    <NodeWidget v-if="iconInput" :input="iconInput" />
    <NodeWidget v-if="props.input.value.rhs" :input="WidgetInput.FromAst(props.input.value.rhs)" />
  </div>
</template>

<style scoped>
.WidgetSelfAccessChain {
  gap: var(--widget-token-pad-unit);
  position: relative;
}

/* Port-like hover area for extended icon port above method name. */
.GraphEditor.draggingEdge .WidgetSelfAccessChain[data-port]::before {
  pointer-events: all;
  content: '';
  position: absolute;
  display: block;
  left: 0;
  right: 0;
  inset: var(--widget-port-drag-inset);
}

/* Feature-flag controlled debug display for hover areas. */
.App.debugHoverAreas .GraphEditor.draggingEdge .WidgetSelfAccessChain[data-port]::before {
  background: rgba(255, 0, 0, 0.3);
}
</style>
