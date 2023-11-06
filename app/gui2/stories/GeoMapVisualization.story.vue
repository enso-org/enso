<script setup lang="ts">
import GeoMapVisualization from '@/components/visualizations/GeoMapVisualization.vue'
import { ref } from 'vue'

const data = ref<any>({
  latitude: 37.8,
  longitude: -122.45,
  zoom: 15,
  controller: true,
  showingLabels: true, // Enables presenting labels when hovering over a point.
  layers: [
    {
      type: 'Scatterplot_Layer',
      data: [
        {
          latitude: 37.8,
          longitude: -122.45,
          color: [255, 0, 0],
          radius: 100,
          label: 'an example label',
        },
      ],
    },
  ],
})
</script>

<template>
  <Story
    title="Geo Map"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    autoPropsDisabled
  >
    <div style="height: 322px">
      <GeoMapVisualization :data="data" />
    </div>

    <template #controls>
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>
