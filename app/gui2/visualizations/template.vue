<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'

import { computed } from 'vue'
import Visualization from './Visualization.vue'

registerVisualization('<visualization name here>', 'Any')

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
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

type Data = [x: number, y: number, r: number][]

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
    <!-- <visualization content here> -->
  </Visualization>
</template>

<style scoped></style>
