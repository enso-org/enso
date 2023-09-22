<script lang="ts">
export const name = 'JSON'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import { onMounted } from 'vue'

import VisualizationContainer from '@/components/VisualizationContainer.vue'

type Data = Record<string, unknown>

const props = defineProps<{ data: Data }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Preprocessor', 'error_preprocessor')
})
</script>

<template>
  <VisualizationContainer :below-toolbar="true">
    <div class="JSONVisualization" v-text="data"></div>
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
