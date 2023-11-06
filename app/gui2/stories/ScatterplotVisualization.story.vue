<script setup lang="ts">
import ScatterplotVisualization from '@/components/visualizations/ScatterplotVisualization.vue'
import { ref } from 'vue'

const data = ref<any>({
  axis: {
    x: { label: 'x-axis label', scale: 'linear' },
    y: { label: 'y-axis label', scale: 'logarithmic' },
  },
  points: { labels: 'visible' },
  data: [
    { x: 0.1, y: 0.7, label: 'foo', color: '#FF0000', shape: 'circle', size: 0.2 },
    { x: 0.4, y: 0.2, label: 'baz', color: '#0000FF', shape: 'square', size: 0.3 },
  ],
})
</script>

<template>
  <Story
    title="Scatterplot"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    autoPropsDisabled
  >
    <div style="height: 322px">
      <ScatterplotVisualization :data="data" />
    </div>

    <template #controls>
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>
