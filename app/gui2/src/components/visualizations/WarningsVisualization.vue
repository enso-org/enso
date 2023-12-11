<script lang="ts">
export const name = 'Warnings'
export const icon = 'exclamation'
export const inputType = 'Any'
export const defaultPreprocessor = [
  'Standard.Visualization.Warnings',
  'process_to_json_text',
] as const
</script>

<script setup lang="ts">
import { VisualizationContainer } from '@/util/visualizationBuiltins'

type Data = string[]

const props = defineProps<{ data: Data }>()
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
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
