<script setup lang="ts">
import { ref } from 'vue'

import HistogramVisualization from '@/components/visualizations/HistogramVisualization.vue'

const data = ref<any>({
  axis: {
    x: { label: 'x-axis label', scale: 'linear' },
    y: { label: 'y-axis label', scale: 'logarithmic' },
  },
  color: 'rgb(1.0,0.0,0.0)',
  bins: 10,
  data: {
    values: [0.1, 0.2, 0.1, 0.15, 0.7],
  },
})
</script>

<template>
  <Story
    title="Histogram"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    autoPropsDisabled
  >
    <div style="height: 322px">
      <HistogramVisualization :data="data" />
    </div>

    <template #controls>
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>
