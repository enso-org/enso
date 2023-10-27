<script lang="ts">
export const name = 'JSON'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import VisualizationContainer from '@/components/VisualizationContainer.vue'

const props = defineProps<{ data: unknown }>()
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <div class="JSONVisualization" v-text="props.data"></div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');

.JSONVisualization {
  font-family: 'DejaVu Sans Mono', monospace;
  white-space: pre;
  padding: 8px;
}
</style>
