<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { AstExtended } from '@/util/ast'
import { ArgumentApplication } from '@/util/callTree'
import { computed } from 'vue'
import { ForcePort } from './WidgetPort.vue'

const props = defineProps(widgetProps(widgetDefinition))
const targetMaybePort = computed(() =>
  props.input.target instanceof AstExtended
    ? new ForcePort(props.input.target)
    : props.input.target,
)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(ArgumentApplication, {
  priority: 1000,
})
</script>

<template>
  <span class="WidgetApplication">
    <NodeWidget :input="targetMaybePort" />
    <NodeWidget v-if="props.input.infixOperator" :input="props.input.infixOperator" />
    <NodeWidget :input="props.input.argument" />
  </span>
</template>

<style>
.WidgetApplication {
  display: inline-flex;
  align-items: center;
  flex-direction: row;
  justify-content: center;
  gap: 4px;
}
</style>
