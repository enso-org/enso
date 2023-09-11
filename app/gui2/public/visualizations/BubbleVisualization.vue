<script lang="ts">
export const name = 'Bubble'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import VisualizationContainer from './VisualizationContainer.vue'

import { computed } from 'vue'

type Data = [x: number, y: number, r: number][]

const props = defineProps<{ data: Data | string }>()

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

// Consider adding more features:
// - Scale data so it is all in view
// - Zooming
// - Hover tooltips
</script>

<template>
  <VisualizationContainer :="<any>$attrs" :below-toolbar="true">
    <svg class="BubbleVisualization">
      <circle
        v-for="([x, y, r], index) in data"
        :key="index"
        stroke="black"
        fill="red"
        :cx="x"
        :cy="y"
        :r="r"
      />
    </svg>
  </VisualizationContainer>
</template>
