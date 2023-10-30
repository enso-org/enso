<script lang="ts">
export const name = 'Error Loading Visualization'
export const inputType = ''
</script>

<script setup lang="ts">
import VisualizationContainer from '@/components/VisualizationContainer.vue'

const _props = defineProps<{ data: unknown }>()
</script>

<template>
  <VisualizationContainer belowToolbar>
    <div class="ErrorLoadingVisualizationVisualization">Error loading visualization.</div>
  </VisualizationContainer>
</template>

<style scoped>
.ErrorLoadingVisualizationVisualization {
  height: 100%;
  padding-top: 30px;
  display: grid;
  place-items: center;
}
</style>
