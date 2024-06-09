<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { FunctionName } from '@/components/GraphEditor/widgets/WidgetFunctionName.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import { WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { useGraphStore } from '@/stores/graph'
import { entryMethodPointer } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { ArgumentApplication, ArgumentApplicationKey } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()
const application = computed(() => props.input[ArgumentApplicationKey])
const graph = useGraphStore()

const targetMaybePort = computed(() => {
  const target = application.value.target
  if (target instanceof Ast.Ast) {
    const input = WidgetInput.FromAst(target)
    if (!application.value.calledFunction) return input
    const ptr = entryMethodPointer(application.value.calledFunction)
    if (!ptr) return input
    const definition = graph.getMethodAst(ptr)
    if (!definition.ok) return input
    input[FunctionName] = {
      editableName: definition.value.name.externalId,
    }
    return input
  } else {
    return { ...target.toWidgetInput(), forcePort: !(target instanceof ArgumentApplication) }
  }
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
export const widgetDefinition = defineWidget(
  ArgumentApplicationKey,
  {
    priority: -20,
  },
  import.meta.hot,
)
</script>

<template>
  <span class="WidgetApplication" :class="appClass">
    <NodeWidget :input="targetMaybePort" />
    <div v-if="application.infixOperator" class="infixOp" :style="operatorStyle">
      <NodeWidget :input="WidgetInput.FromAst(application.infixOperator)" />
    </div>
    <SizeTransition width leftGap>
      <div v-if="tree.extended || !application.argument.hideByDefault" class="argument">
        <NodeWidget :input="application.argument.toWidgetInput()" nest />
      </div>
    </SizeTransition>
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
}
</style>
