<script setup lang="ts">
import { escapeHtml } from '@/util/escapeHtml'
import { registerVisualization } from '@/util/visualizations'

import { computed, onMounted } from 'vue'

/** Simple Warning Visualization. */

registerVisualization('Warnings', 'Any')

const props = defineProps<{
  width: number
  height: number
  data: Data | string
}>()

type Data = string[]

onMounted(() => {
  setPreprocessor('Standard.Visualization.Warnings', 'process_to_json_text')
})

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <div class="Warnings visualization" :width="width" :height="height">
    <ul style="font-family: DejaVuSansMonoBook, sans-serif; font-size: 12px; white-space: pre">
      <li v-if="data.length === 0">There are no warnings.</li>
      <li v-for="warning in data" v-text="escapeHtml(warning)"></li>
    </ul>
  </div>
</template>

<style scoped></style>
