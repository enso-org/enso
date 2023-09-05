<script setup lang="ts">
import FindIcon from './GeoMap/find.svg'
import Path2Icon from './GeoMap/path2.svg'
import GeoMapDistanceIcon from './GeoMap/geo_map_distance.svg'
import GeoMapPinIcon from './GeoMap/geo_map_pin.svg'

import Visualization from '@/components/Visualization.vue'
import { registerVisualization } from '@/util/visualizations'

import { computed } from 'vue'

registerVisualization('GeoMap', 'Any')

const props = defineProps<{
  width: number
  height: number
  data: Data | string
}>()

type Data = [x: number, y: number, r: number][]

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

// FIXME: impl
</script>

<template>
  <Visualization>
    <template #toolbar>
      <button class="button"><img :src="FindIcon" /></button>
      <button class="button"><img :src="Path2Icon" /></button>
      <button class="button"><img :src="GeoMapDistanceIcon" /></button>
      <button class="button"><img :src="GeoMapPinIcon" /></button>
    </template>
    <svg #visualization class="GeoMap" :width="width" :height="height">
      <circle v-for="[x, y, r] in data" stroke="black" fill="red" :cx="x" :cy="y" :r="r"></circle>
    </svg>
  </Visualization>
</template>

<style scoped></style>
