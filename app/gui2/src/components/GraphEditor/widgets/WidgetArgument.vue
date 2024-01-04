<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { ArgumentAst, ArgumentPlaceholder } from '@/util/callTree'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget([ArgumentAst, ArgumentPlaceholder], {
  priority: 2000,
})
</script>

<template>
  <NodeWidget :input="props.input.toAnyWidget()" nest />
</template>
