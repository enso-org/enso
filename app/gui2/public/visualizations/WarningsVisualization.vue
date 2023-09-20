<script lang="ts">
export const name = 'Warnings'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import VisualizationContainer from 'builtins/VisualizationContainer.vue'

import { computed, onMounted } from 'vue'

/** Simple Warning Visualization. */

type Data = string[]

const props = defineProps<{ data: Data | string | undefined }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Warnings', 'process_to_json_text')
})

const data = computed<Data | undefined>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <VisualizationContainer :below-toolbar="true">
    <div class="WarningsVisualization">
      <ul v-if="data">
        <li v-if="data.length === 0">There are no warnings.</li>
        <li v-for="(warning, index) in data" :key="index" v-text="warning"></li>
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
