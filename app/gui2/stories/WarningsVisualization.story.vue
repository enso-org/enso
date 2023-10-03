<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import WarningsVisualization from '@/components/visualizations/WarningsVisualization.vue'

const data = ref<any>(['warning 1', "warning 2!!&<>;'\x22"])
</script>

<template>
  <Story
    title="Warnings"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    auto-props-disabled
  >
    <div style="height: 322px">
      <WarningsVisualization :data="data" />
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
