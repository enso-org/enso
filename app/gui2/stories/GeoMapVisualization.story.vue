<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import { onMounted } from 'vue'
import GeoMapVisualization from '../public/visualizations/GeoMapVisualization.vue'

const DEPENDENCIES = [
  'https://api.tiles.mapbox.com/mapbox-gl-js/v2.15.0/mapbox-gl.js',
  'https://cdn.jsdelivr.net/npm/deck.gl@8.9.27/dist.min.js',
]

const ready = ref(false)

onMounted(async () => {
  await Promise.allSettled(
    DEPENDENCIES.map(
      (scriptUrl) =>
        new Promise<void>((resolve, reject) => {
          const script = document.createElement('script')
          script.src = scriptUrl
          script.addEventListener('load', () => {
            resolve()
            script.remove()
          })
          script.addEventListener('error', () => {
            reject()
            script.remove()
          })
          document.body.appendChild(script)
        }),
    ),
  )
  ready.value = true
})

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
    auto-props-disabled
  >
    <div style="height: 250px">
      <GeoMapVisualization v-if="ready" :data="data" />
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
