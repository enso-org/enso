<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'

import { computed } from 'vue'
import Visualization from './Visualization.vue'

registerVisualization('Bubble', 'Any')

const props = defineProps<{ isCircularMenuVisible: boolean; data: Data | string }>()
const emit = defineEmits<{ hide: [] }>()

type Data = [x: number, y: number, r: number][]

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <Visualization :is-circular-menu-visible="isCircularMenuVisible" @hide="emit('hide')">
    <svg class="Bubble">
      <circle v-for="[x, y, r] in data" stroke="black" fill="red" :cx="x" :cy="y" :r="r"></circle>
    </svg>
  </Visualization>
</template>

<style scoped></style>
