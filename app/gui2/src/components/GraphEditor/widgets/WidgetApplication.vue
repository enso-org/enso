<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { ArgumentApplication, ArgumentApplicationKey } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()
const application = computed(() => props.input[ArgumentApplicationKey])
const targetMaybePort = computed(() => {
  const target = application.value.target
  const targetInput =
    target instanceof Ast.Ast ? WidgetInput.FromAst(target) : target.toWidgetInput()
  return { ...targetInput, forcePort: !(target instanceof ArgumentApplication) }
})

const appClass = computed(() => {
  return application.value.infixOperator != null ? 'infix' : 'prefix'
})

const operatorStyle = computed(() => {
  if (
    application.value.appTree instanceof Ast.OprApp ||
    application.value.appTree instanceof Ast.PropertyAccess
  ) {
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
    <div
      v-if="tree.extended || !application.argument.hideByDefault"
      :class="{ animateWhenShown: application.argument.hideByDefault }"
    >
      <NodeWidget :input="application.argument.toWidgetInput()" nest />
    </div>
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

.animateWhenShown {
  animation: show 4800ms 100ms cubic-bezier(0.38, 0.97, 0.56, 0.76) forwards;
  max-width: 0;
  overflow-x: clip;
}

@keyframes show {
  100% {
    max-width: 2000px;
  }
}
</style>
