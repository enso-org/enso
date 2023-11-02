<script lang="ts">
export const name = 'Loading Error'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { watchEffect } from 'vue'

const props = defineProps<{ data: { name: string; error: Error } }>()

watchEffect(() => console.error(props.data.error))
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <div class="LoadingErrorVisualization">
      <div>
        <span>Could not load visualization '<span v-text="props.data.name"></span>':</span>
        <span v-text="props.data.error.message"></span>
      </div>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
.LoadingErrorVisualization {
  height: 100%;
  display: grid;
  place-items: center;
}

.LoadingErrorVisualization > div {
  display: flex;
  flex-flow: column;
  place-items: center;
  text-align: center;
}
</style>
