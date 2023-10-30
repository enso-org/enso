<script lang="ts">
export const name = 'Error Loading Visualization'
export const inputType = ''
</script>

<script setup lang="ts">
import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { watchEffect } from 'vue'

const props = defineProps<{ data: { name: string; error: Error } }>()

watchEffect(() => console.error(props.data))
</script>

<template>
  <VisualizationContainer belowToolbar>
    <div class="ErrorLoadingVisualizationVisualization">
      <div>
        <span>Error loading visualization '<span v-text="props.data.name"></span>':</span>
        <span v-text="props.data.error.message"></span>
      </div>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
.ErrorLoadingVisualizationVisualization {
  height: 100%;
  display: grid;
  place-items: center;
}

.ErrorLoadingVisualizationVisualization > div {
  display: flex;
  flex-flow: column;
  place-items: center;
}
</style>
