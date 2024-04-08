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
    <Transition name="collapse-argument">
      <div
        v-if="tree.extended || !application.argument.hideByDefault"
        class="argument overridableClipState"
      >
        <NodeWidget :input="application.argument.toWidgetInput()" nest />
      </div>
    </Transition>
  </span>
</template>

<style scoped>
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

.argument {
  display: flex;
  flex-direction: row;
  place-items: center;
  max-width: 2000px;
}

.collapse-argument-enter-active {
  transition: max-width 4800ms 100ms cubic-bezier(0.38, 0.97, 0.56, 0.76);
}

.collapse-argument-leave-active {
  transition: max-width 0.5s cubic-bezier(0, 0.76, 0, 0.99);
}

/* Clipping is part of the show/hide animation, but must not be applied when the argument is fully-shown because
   attachments such as dropdowns extend beyond the element and may be children of the argument. */
.collapse-argument-enter-active,
.collapse-argument-leave-active,
.collapse-argument-enter-from,
.collapse-argument-leave-to {
  overflow-x: clip;
}

.collapse-argument-enter-from,
.collapse-argument-leave-to {
  max-width: 0;
}
</style>
