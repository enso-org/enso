<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import type { WidgetInput } from '@/providers/widgetRegistry'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import type { Ast } from '@/util/ast'
import { SoCalledExpression } from '@/util/callTree'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
function isExistingArg(input: WidgetInput): input is SoCalledExpression & { ast: Ast.Ast } {
  return input instanceof SoCalledExpression && input.ast != null
}

export const widgetDefinition = defineWidget(isExistingArg, {
  priority: 1001,
})
</script>

<template>
  <NodeWidget :input="props.input.ast" nest />
</template>
