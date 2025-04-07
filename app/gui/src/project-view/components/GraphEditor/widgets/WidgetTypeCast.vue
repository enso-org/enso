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

const annotatedExpression = computed<Ast.TypeAnnotated>(() => {
  if (
    props.input.value instanceof Ast.Group &&
    props.input.value.expression instanceof Ast.TypeAnnotated
  ) {
    return props.input.value.expression
  } else {
    return props.input.value as Ast.TypeAnnotated
  }
})

const expressionInput = computed(() =>
  WidgetInput.WithPort(WidgetInput.FromAst(annotatedExpression.value.expression)),
)
const typeNodeInput = computed(() => WidgetInput.FromAst(annotatedExpression.value.typeNode))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [WidgetInput.astMatcher(Ast.TypeAnnotated), WidgetInput.astMatcher(Ast.Group)],
  {
    priority: 1000,
    score: (info) => {
      // Only groups with TypeAnnotated expression are valid.
      if (
        info.input.value instanceof Ast.Group &&
        !(info.input.value.expression instanceof Ast.TypeAnnotated)
      ) {
        return Score.Mismatch
      } else {
        return Score.Perfect
      }
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTypeCast">
    <NodeWidget v-if="!isSelfArgument" :input="expressionInput" />
    <span class="typeAnnotation">:</span>
    <NodeWidget class="typeAnnotation" :input="typeNodeInput" />
  </div>
</template>

<style scoped>
.WidgetTypeCast {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 0;
}

.typeAnnotation {
  opacity: 0.6;
}
</style>
