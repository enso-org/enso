<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import HeatmapVisualization from '@/components/visualizations/HeatmapVisualization.vue'

const data = ref<any>([
  ['A', 'B', 'C', 'D', 'A'],
  [1, 2, 3, 2, 3],
  [50, 25, 40, 20, 10],
])
</script>

<template>
  <Story
    title="Heatmap"
    group="visualizations"
    :layout="{ type: 'grid', width: 250 }"
    auto-props-disabled
  >
    <div style="height: 222px">
      <HeatmapVisualization :data="data" />
    </div>

    <template #controls>
      <HstJson v-model="data" />
    </template>
  </Story>
</template>

<style>
.VisualizationContainer {
  z-index: 0 !important;
  background: #2222;
}
</style>
