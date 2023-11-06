<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, defineWidget } from '@/providers/widgetRegistry'
import type { AstExtended } from '@/util/ast'

const props = defineProps<{ ast: AstExtended }>()
</script>

<script lang="ts">
export const widgetConfig = defineWidget({
  beforeOverride: true,
  priority: 1000,
  match: (info) => {
    const isConnected = info.db.connections.reverseLookup(info.ast.astId).size > 0
    if (isConnected) {
      return Score.Perfect
    }
    return Score.Mismatch
  },
})
</script>
<template>
  <span class="WidgetPort"><NodeWidget :ast="props.ast" /></span>
</template>

<style scoped>
.WidgetPort {
  display: inline-block;
  vertical-align: middle;
  border-radius: var(--radius-full);
  min-height: 24px;
  background-color: var(--node-color-port);
  padding: 0 2px;
  transition: background-color 0.2s ease;
}
</style>
