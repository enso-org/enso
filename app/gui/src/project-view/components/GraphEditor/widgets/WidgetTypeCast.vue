<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()

const isSelfArgument = computed(() => {
  const input = props.input.value
  const selfId =
    input instanceof Ast.Group ? input.expression?.id
    : input instanceof Ast.TypeAnnotated ? input.expression.id
    : undefined
  return selfId === tree.potentialSelfArgumentId
})

const input = computed(() => {
  if (
    props.input.value instanceof Ast.Group &&
    props.input.value.expression instanceof Ast.TypeAnnotated
  ) {
    return props.input.value.expression
  } else if (props.input.value instanceof Ast.TypeAnnotated) {
    return props.input.value
  }
  return undefined
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [WidgetInput.astMatcher(Ast.TypeAnnotated), WidgetInput.astMatcher(Ast.Group)],
  {
    priority: 1000,
    score: (info) => {
      if (
        info.input.value instanceof Ast.Group &&
        info.input.value.expression instanceof Ast.TypeAnnotated
      ) {
        return Score.Perfect
      } else if (info.input.value instanceof Ast.TypeAnnotated) {
        return Score.Perfect
      } else {
        return Score.Mismatch
      }
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTypeCast">
    <NodeWidget
      v-if="input && !isSelfArgument"
      :input="WidgetInput.WithPort(WidgetInput.FromAst(input?.expression))"
    />
    <span class="token">:</span>
    <NodeWidget class="token" v-if="input" :input="WidgetInput.FromAst(input?.typeNode)" />
  </div>
</template>

<style scoped>
.WidgetTypeCast {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 0;
}

.token {
  opacity: 0.6;
}
</style>
