<script setup lang="ts">
import FindIcon from './GeoMap/find.svg'
import Path2Icon from './GeoMap/path2.svg'
import GeoMapDistanceIcon from './GeoMap/geo_map_distance.svg'
import GeoMapPinIcon from './GeoMap/geo_map_pin.svg'

import Visualization from './Visualization.vue'
import { registerVisualization } from '@/util/visualizations'

import { computed } from 'vue'

registerVisualization('GeoMap', 'Any')

type Data = [x: number, y: number, r: number][]

const props = defineProps<{
  isCircularMenuVisible: boolean
  width: number
  height: number
  fullscreen: boolean
  data: Data | string
}>()
const emit = defineEmits<{
  hide: []
  'update:width': [width: number]
  'update:height': [height: number]
  'update:fullscreen': [fullscreen: boolean]
}>()

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

// FIXME: impl
</script>

<template>
  <Visualization
    :data="data"
    @hide="emit('hide')"
    :is-circular-menu-visible="isCircularMenuVisible"
    :width="width"
    @update:width="emit('update:width', $event)"
    :height="height"
    @update:height="emit('update:height', $event)"
    :fullscreen="fullscreen"
    @update:fullscreen="emit('update:fullscreen', $event)"
  >
    <template #toolbar>
      <button class="button"><img :src="FindIcon" /></button>
      <button class="button"><img :src="Path2Icon" /></button>
      <button class="button"><img :src="GeoMapDistanceIcon" /></button>
      <button class="button"><img :src="GeoMapPinIcon" /></button>
    </template>
  </Visualization>
</template>

<style scoped></style>
