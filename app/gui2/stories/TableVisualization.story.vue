<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import TableVisualization from '@/components/visualizations/TableVisualization.vue'

const data = ref<any>({
  type: 'Matrix',
  // eslint-disable-next-line camelcase
  column_count: 5,
  // eslint-disable-next-line camelcase
  all_rows_count: 10,
  json: Array.from({ length: 10 }, (_, i) => Array.from({ length: 5 }, (_, j) => `${i},${j}`)),
})
</script>

<template>
  <Story
    title="Table"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    auto-props-disabled
  >
    <div style="height: 322px">
      <TableVisualization :data="data" />
    </div>

    <template #controls>
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>

<style>
.VisualizationContainer {
  z-index: 0 !important;
  min-width: 0 !important;
}
</style>
