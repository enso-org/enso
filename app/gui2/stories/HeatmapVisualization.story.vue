<script setup lang="ts">
import { ref } from 'vue'

import HeatmapVisualization from '@/components/visualizations/HeatmapVisualization.vue'

const data = ref<any>([
  ['A', 'B', 'C', 'D', 'A'],
  ['D', 'E', 'D', 'X', 'Z'],
  [50, 25, 40, 20, 10],
])
</script>

<template>
  <Story
    title="Heatmap"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    autoPropsDisabled
  >
    <div style="height: 322px">
      <HeatmapVisualization :data="data" />
    </div>

    <template #controls>
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>
