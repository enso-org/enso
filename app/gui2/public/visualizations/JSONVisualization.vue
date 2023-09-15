<script lang="ts">
export const name = 'JSON'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import VisualizationContainer from './VisualizationContainer.vue'

import { onMounted } from 'vue'

type Data = Record<string, unknown>

const props = defineProps<{ data: Data | string | undefined }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Preprocessor', 'error_preprocessor')
})
</script>

<template>
  <VisualizationContainer :="<any>$attrs" :below-toolbar="true">
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
