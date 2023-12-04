<script lang="ts">
import LoadingSpinner from '@/components/LoadingSpinner.vue'
import { VisualizationContainer } from '@/util/visualizationBuiltins'

export const name = 'Loading'
export const inputType = 'Any'
</script>

<script setup lang="ts">
const _props = defineProps<{ data: unknown }>()
</script>

<template>
  <VisualizationContainer>
    <div class="LoadingVisualization">
      <LoadingSpinner />
    </div>
  </VisualizationContainer>
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
