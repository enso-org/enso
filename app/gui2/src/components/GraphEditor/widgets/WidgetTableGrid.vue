<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
import { Ast } from '@/util/ast'

export const widgetDefinition = defineWidget(
  WidgetInput.isFunctionCall,
  {
    priority: 1,
    score: (props, db) => {
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTableGrid">
    <!-- <NodeWidget v-if="child" :input="child" /> -->
  </div>
</template>

<style scoped>
.WidgetTableGrid {
  display: flex;
  align-items: center;
  min-height: 200px;
  min-width: 200px;
  background: tan;
}
</style>
