<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'
import Visualization from './Visualization.vue'

import { computed, onMounted } from 'vue'

/** Simple Warning Visualization. */

registerVisualization('Warnings', 'Any')

type Data = string[]

const props = defineProps<{ data: Data | string }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Warnings', 'process_to_json_text')
})

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <Visualization :="<any>$attrs" below-toolbar>
    <div class="Warnings">
      <ul style="font-family: DejaVuSansMonoBook, sans-serif; font-size: 12px; white-space: pre">
        <li v-if="data.length === 0">There are no warnings.</li>
        <li v-for="warning in data" v-text="warning"></li>
      </ul>
    </div>
  </Visualization>
</template>

<style scoped>
.Warnings {
  padding: 8px;
}

ul {
  padding-inline-start: 0;
}

li {
  list-style: none;
}
</style>
