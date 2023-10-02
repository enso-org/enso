<script lang="ts">
export const name = 'Warnings'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import { onMounted } from 'vue'

import VisualizationContainer from '@/components/VisualizationContainer.vue'

type Data = string[]

const props = defineProps<{ data: Data }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Warnings', 'process_to_json_text')
})
</script>

<template>
  <VisualizationContainer :below-toolbar="true">
    <div class="WarningsVisualization">
      <ul>
        <li v-if="props.data.length === 0">There are no warnings.</li>
        <li v-for="(warning, index) in props.data" :key="index" v-text="warning"></li>
      </ul>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
.WarningsVisualization {
  padding: 8px;
}

ul {
  white-space: pre;
  padding-inline-start: 0;
}

li {
  list-style: none;
}
</style>
