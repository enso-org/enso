<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'
import Visualization from './Visualization.vue'

import { computed } from 'vue'

registerVisualization('Bubble', 'Any')

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
    <svg class="Bubble">
      <circle v-for="[x, y, r] in data" stroke="black" fill="red" :cx="x" :cy="y" :r="r"></circle>
    </svg>
  </Visualization>
</template>

<style scoped></style>
