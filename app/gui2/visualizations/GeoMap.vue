<script setup lang="ts">
import FindIcon from './GeoMap/find.svg'
import Path2Icon from './GeoMap/path2.svg'
import GeoMapDistanceIcon from './GeoMap/geo_map_distance.svg'
import GeoMapPinIcon from './GeoMap/geo_map_pin.svg'

import Visualization from '@viz/Visualization.vue'
import { registerVisualization } from '@/util/visualizations'

import { computed } from 'vue'

registerVisualization('GeoMap', 'Any')

const props = defineProps<{ isCircularMenuVisible: boolean; data: Data | string }>()
const emit = defineEmits<{ hide: [] }>()

type Data = [x: number, y: number, r: number][]

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

// FIXME: impl
</script>

<template>
  <Visualization :is-circular-menu-visible="isCircularMenuVisible" @hide="emit('hide')">
    <template #toolbar>
      <button class="button"><img :src="FindIcon" /></button>
      <button class="button"><img :src="Path2Icon" /></button>
      <button class="button"><img :src="GeoMapDistanceIcon" /></button>
      <button class="button"><img :src="GeoMapPinIcon" /></button>
    </template>
  </Visualization>
</template>

<style scoped></style>
