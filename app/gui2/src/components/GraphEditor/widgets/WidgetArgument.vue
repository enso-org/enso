<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { ArgumentAst } from '@/util/callTree'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(ArgumentAst, {
  priority: 1001,
})
</script>

<template>
  <NodeWidget :input="props.input.ast" nest />
</template>
