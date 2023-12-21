<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import type { WidgetInput } from '@/providers/widgetRegistry'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import type { Ast } from '@/util/ast'
import { Argument } from '@/util/callTree'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
function isExistingArg(input: WidgetInput): input is Argument & { ast: Ast.Ast } {
  return input instanceof Argument && input.ast != null
}

export const widgetDefinition = defineWidget(isExistingArg, {
  priority: 1001,
})
</script>

<template>
  <NodeWidget :input="props.input.toAnyWidget()" nest />
</template>
