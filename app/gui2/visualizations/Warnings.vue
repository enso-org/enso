<script setup lang="ts">
import { escapeHtml } from '@/util/escapeHtml'
import { registerVisualization } from '@/util/visualizations'

import { computed, onMounted } from 'vue'
import Visualization from './Visualization.vue'

/** Simple Warning Visualization. */

registerVisualization('Warnings', 'Any')

const props = defineProps<{
  isCircularMenuVisible: boolean
  data: Data | string
}>()
const emit = defineEmits<{
  hide: []
  'update:preprocessor': [module: string, method: string]
}>()

type Data = string[]

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Warnings', 'process_to_json_text')
})

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <Visualization
    dodge-buttons
    :is-circular-menu-visible="isCircularMenuVisible"
    @hide="emit('hide')"
  >
    <div class="Warnings">
      <ul style="font-family: DejaVuSansMonoBook, sans-serif; font-size: 12px; white-space: pre">
        <li v-if="data.length === 0">There are no warnings.</li>
        <li v-for="warning in data" v-text="escapeHtml(warning)"></li>
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
