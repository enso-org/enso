<script setup lang="ts">
import FindIcon from './GeoMap/find.svg'
import Path2Icon from './GeoMap/path2.svg'
import GeoMapDistanceIcon from './GeoMap/geo_map_distance.svg'
import GeoMapPinIcon from './GeoMap/geo_map_pin.svg'

import VisualizationContainer from './VisualizationContainer.vue'

import { computed } from 'vue'

type Data = [x: number, y: number, r: number][]

const props = defineProps<{ data: Data | string }>()

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

// FIXME: impl
</script>

<template>
  <VisualizationContainer :="<any>$attrs">
    <template #toolbar>
      <button class="button"><img :src="FindIcon" /></button>
      <button class="button"><img :src="Path2Icon" /></button>
      <button class="button"><img :src="GeoMapDistanceIcon" /></button>
      <button class="button"><img :src="GeoMapPinIcon" /></button>
    </template>
  </VisualizationContainer>
</template>

<style scoped></style>
