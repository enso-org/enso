<script lang="ts">
export const name = 'Geo Map'
export const inputType = 'Standard.Table.Data.Table.Table'
</script>

<script setup lang="ts">
import FindIcon from './icons/find.svg'
import Path2Icon from './icons/path2.svg'
import GeoMapDistanceIcon from './icons/geo_map_distance.svg'
import GeoMapPinIcon from './icons/geo_map_pin.svg'

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
      <button class="image-button"><img :src="FindIcon" /></button>
      <button class="image-button"><img :src="Path2Icon" /></button>
      <button class="image-button"><img :src="GeoMapDistanceIcon" /></button>
      <button class="image-button"><img :src="GeoMapPinIcon" /></button>
    </template>
  </VisualizationContainer>
</template>

<style scoped></style>
