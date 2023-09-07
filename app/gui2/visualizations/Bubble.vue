<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'
import Visualization from './Visualization.vue'

import { computed } from 'vue'

registerVisualization('Bubble', 'Any')

type Data = [x: number, y: number, r: number][]

const props = defineProps<{ data: Data | string }>()

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <Visualization :="<any>$attrs">
    <svg class="Bubble">
      <circle v-for="[x, y, r] in data" stroke="black" fill="red" :cx="x" :cy="y" :r="r"></circle>
    </svg>
  </Visualization>
</template>

<style scoped></style>
