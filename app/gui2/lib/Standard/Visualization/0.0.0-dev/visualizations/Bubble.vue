<script setup lang="ts">
import Visualization from './Visualization.vue'

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
  <Visualization :="<any>$attrs" :below-toolbar="true">
    <svg class="Bubble">
      <circle v-for="[x, y, r] in data" stroke="black" fill="red" :cx="x" :cy="y" :r="r" />
    </svg>
  </Visualization>
</template>

<style scoped></style>
