<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { ArgumentApplicationKey, ArgumentAst, ArgumentPlaceholder } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const application = computed(() => props.input[ArgumentApplicationKey])
const targetMaybePort = computed(() => {
  const target = application.value.target
  return target instanceof ArgumentPlaceholder || target instanceof ArgumentAst
    ? { ...target.toWidgetInput(), forcePort: true }
    : target instanceof Ast.Ast
    ? WidgetInput.FromAst(target)
    : target.toWidgetInput()
})

const appClass = computed(() => {
  return application.value.infixOperator != null ? 'infix' : 'prefix'
})

const operatorStyle = computed(() => {
  if (application.value.appTree instanceof Ast.OprApp) {
    const [_lhs, opr, rhs] = application.value.appTree.concreteChildren()
    return {
      '--whitespace-pre': `${JSON.stringify(opr?.whitespace ?? '')}`,
      '--whitespace-post': `${JSON.stringify(rhs?.whitespace ?? '')}`,
    }
  }
  return {}
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(ArgumentApplicationKey, {
  priority: -20,
})
</script>

<template>
  <span class="WidgetApplication" :class="appClass">
    <NodeWidget :input="targetMaybePort" />
    <div v-if="application.infixOperator" class="infixOp" :style="operatorStyle">
      <NodeWidget :input="WidgetInput.FromAst(application.infixOperator)" />
    </div>
    <NodeWidget :input="application.argument.toWidgetInput()" nest />
  </span>
</template>

<style>
.WidgetApplication {
  display: flex;
  align-items: center;
  flex-direction: row;
  justify-content: center;
  &.prefix {
    gap: 4px;
  }
}

.infixOp {
  display: flex;

  &:before {
    content: var(--whitespace-pre);
    display: inline;
    white-space: pre;
  }

  &:after {
    content: var(--whitespace-post);
    display: inline;
    white-space: pre;
  }
}
</style>
