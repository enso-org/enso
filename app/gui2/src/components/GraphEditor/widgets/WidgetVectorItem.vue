<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { VectorItem } from '@/components/GraphEditor/widgets/WidgetVector.vue'
import { defineWidget, widgetProps } from '@/providers/widgetRegistry'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
/** A widget that simply unwraps `VectorItem`.
 * It is needed to allow attaching any other widget to the vector items.
 */
export const widgetDefinition = defineWidget(VectorItem, {
  priority: 2000,
})
</script>

<template>
  <NodeWidget :input="props.input.inner" :dynamicConfig="props.config" nest />
</template>
