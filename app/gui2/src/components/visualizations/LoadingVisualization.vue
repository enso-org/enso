<script lang="ts">
import LoadingSpinner from '@/components/LoadingSpinner.vue'

export const name = 'Loading'
export const inputType = 'Any'
</script>

<script setup lang="ts">
const _props = defineProps<{ data: unknown }>()
</script>

<template>
  <div class="LoadingVisualization">
    <LoadingSpinner />
  </div>
</template>

<style scoped>
.LoadingVisualization {
  display: flex;
  height: 100%;
  padding-top: 30px;
  place-content: center;
  place-items: center;
  overflow: clip;
}
</style>
