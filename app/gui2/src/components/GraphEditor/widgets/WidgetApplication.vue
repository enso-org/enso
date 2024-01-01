<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { ForcePort } from '@/providers/portInfo'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { ArgumentApplication } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const targetMaybePort = computed(() =>
  props.input.target instanceof Ast.Ast ? new ForcePort(props.input.target) : props.input.target,
)

const appClass = computed(() => {
  return props.input.infixOperator != null ? 'infix' : 'prefix'
})

const operatorStyle = computed(() => {
  if (props.input.appTree instanceof Ast.OprApp) {
    const [_lhs, opr, rhs] = props.input.appTree.concreteChildren()
    return {
      '--whitespace-pre': `${JSON.stringify(opr?.whitespace ?? '')}`,
      '--whitespace-post': `${JSON.stringify(rhs?.whitespace ?? '')}`,
    }
  }
  return {}
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(ArgumentApplication, {
  priority: 1000,
})
</script>

<template>
  <span class="WidgetApplication" :class="appClass">
    <NodeWidget :input="targetMaybePort" />
    <div v-if="props.input.infixOperator" class="infixOp" :style="operatorStyle">
      <NodeWidget :input="props.input.infixOperator" />
    </div>
    <NodeWidget :input="props.input.argument" :dynamicConfig="props.config" />
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
