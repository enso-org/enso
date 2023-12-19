<script lang="ts">
export const name = 'JSON'
export const icon = 'braces'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import { VisualizationContainer } from '@/util/visualizationBuiltins'

const props = defineProps<{ data: unknown }>()
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <div class="JSONVisualization" v-text="props.data"></div>
  </VisualizationContainer>
</template>

<style scoped>
.JSONVisualization {
  font-family: var(--font-mono);
  white-space: pre;
  padding: 8px;
}
</style>
