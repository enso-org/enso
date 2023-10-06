<script lang="ts">
export const name = 'Loading'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import VisualizationContainer from '@/components/VisualizationContainer.vue'

const _props = defineProps<{ data: unknown }>()
</script>

<template>
  <VisualizationContainer>
    <div class="LoadingVisualization"></div>
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

.LoadingVisualization::before {
  content: '';
  display: block;
  width: 30px;
  height: 30px;
  border-radius: 50%;
  border: 4px solid;
  border-color: rgba(0, 0, 0, 30%) #0000;
  animation: s1 0.8s infinite;
}
@keyframes s1 {
  to {
    transform: rotate(0.5turn);
  }
}
</style>
