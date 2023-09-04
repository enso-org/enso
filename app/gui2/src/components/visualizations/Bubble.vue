<script setup lang="ts">
import { registerVizualization } from '@/util/visualizations'

import { computed } from 'vue'

declare module '@/util/visualizations' {
  export interface Visualizations {
    Bubble: 'Any'
  }
}

registerVizualization('Bubble', 'Any')

const props = defineProps<{
  width: number
  height: number
  data: Data | string
}>()

type Data = [x: number, y: number, r: number][]

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data
)
</script>

<template>
  <svg class="Bubble" :width="width" :height="height">
    <circle v-for="[x, y, r] in data" stroke="black" fill="red" :cx="x" :cy="y" :r="r"></circle>
  </svg>
</template>

<style>
.Image {
  max-height: 100%;
  max-width: 100%;
  border-radius: 14px;
}
</style>