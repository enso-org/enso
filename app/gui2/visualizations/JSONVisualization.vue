<script lang="ts">
export const name = 'JSON'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import VisualizationContainer from './VisualizationContainer.vue'

import { onMounted } from 'vue'

type Data = string

const props = defineProps<{ data: Data | string }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Preprocessor', 'error_preprocessor')
})
</script>

<template>
  <VisualizationContainer :="<any>$attrs" :below-toolbar="true">
    <div class="JSON" v-text="data"></div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');

.JSON {
  font-family: DejaVuSansMonoBook, monospace;
  white-space: pre;
  padding: 8px;
}
</style>
