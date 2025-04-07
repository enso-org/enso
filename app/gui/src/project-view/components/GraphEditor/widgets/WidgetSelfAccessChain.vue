<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import { injectFunctionInfo } from '@/providers/functionInfo'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
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
    functionInfo?.outputType ?? 'Unknown',
  )
})
const { displayedIcon } = useDisplayedIcon(graph.db, toRef(tree, 'externalId'), baseIcon)

const iconInput = computed(() => {
  const lhs = props.input.value.lhs
  if (!lhs) return
  // The PortId of the type annotated expression is the internal expression,
  // not the whole Ast.TypeAnnotated, so that the connections are displayed correctly.
  const portId =
    lhs instanceof Ast.Group && lhs.expression instanceof Ast.TypeAnnotated ?
      lhs.expression.expression.id
    : lhs instanceof Ast.TypeAnnotated ? lhs.expression.id
    : lhs.id
  const expression = lhs instanceof Ast.Group && lhs.expression ? lhs.expression : lhs
  const input = WidgetInput.WithPort(WidgetInput.FromAstWithPortId(expression, portId))
  const isTypeAnnotated = lhs instanceof Ast.Group || lhs instanceof Ast.TypeAnnotated
  const showContents = isTypeAnnotated ? true : showFullAccessChain.value
  input[DisplayIcon] = { icon: displayedIcon.value, showContents, noGap: isTypeAnnotated }
  return input
})

// Do not trim calls starting with capital letter. Those are usually "static dispatches", and we
// don't want to hide them. Does not check actual method suggestion info to avoid flickering before
// expression info is loaded. We are already scoped to simple access chain in self position, so
// this check should be accurate in practice.
const showFullAccessChain = computed(() => /^[A-Z]/.test(props.input.value.lhs?.code() ?? ''))
</script>

<script lang="ts">
/** Subject of the self access chain can be either a regular expression, or a type annotated expression. */
const extractSubject = (accessChain: Ast.PropertyAccess | undefined) => {
  const lhs = accessChain?.lhs
  if (lhs instanceof Ast.Group && lhs.expression instanceof Ast.TypeAnnotated) {
    return lhs.expression.expression
  } else if (lhs instanceof Ast.TypeAnnotated) {
    return lhs.expression
  }
  return lhs
}

export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.PropertyAccess),
  {
    priority: 999,
    score: (info) => {
      const tree = injectWidgetTree()
      const selfId = tree.potentialSelfArgumentId
      const subject = extractSubject(info.input.value)
      if (selfId != null) {
        if (subject?.id === selfId) {
          return Score.Good
        }
      }
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelfAccessChain" :class="{ showFullAccessChain }">
    <NodeWidget v-if="iconInput" :input="iconInput" />
    <NodeWidget
      v-if="showFullAccessChain"
      :input="WidgetInput.FromAst(props.input.value.operator)"
    />
    <NodeWidget v-if="props.input.value.rhs" :input="WidgetInput.FromAst(props.input.value.rhs)" />
  </div>
</template>

<style scoped>
.WidgetSelfAccessChain {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: var(--widget-token-pad-unit);
  &.showFullAccessChain {
    gap: 0;
  }
}
</style>
