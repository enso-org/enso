<script setup lang="ts">
import {
  defineWidget,
  Score,
  WidgetInput,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { computed } from 'vue'
import { IsTypeCastKey } from './WidgetTypeCastPort.vue'

const props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()

const isSelfArgument = computed(() => {
  if (!(props.input.value instanceof Ast.TypeAnnotated)) return false
  const selfArgumentId = tree.primaryApplication.selfArgument
  return props.input.value.expression.id === selfArgumentId
})

const expressionInput = computed(() => {
  if (props.input.value instanceof Ast.TypeAnnotated)
    return WidgetInput.FromAst(props.input.value.expression)
  else return undefined
})
const typeNodeInput = computed(() => {
  if (props.input.value instanceof Ast.TypeAnnotated)
    return WidgetInput.FromAst(props.input.value.typeNode)
  else return undefined
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  IsTypeCastKey,
  {
    priority: 1,
    score: () => Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTypeCast widgetParent">
    <NodeWidget v-if="expressionInput && !isSelfArgument" :input="expressionInput" />
    <span class="typeAnnotation widgetSingleLine">:</span>
    <NodeWidget v-if="typeNodeInput" class="typeAnnotation" :input="typeNodeInput" />
  </div>
</template>

<style scoped>
.WidgetTypeCast {
  gap: 0;
}

.typeAnnotation {
  opacity: 0.6;
}
</style>
