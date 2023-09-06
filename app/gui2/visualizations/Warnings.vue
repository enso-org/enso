<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'
import Visualization from './Visualization.vue'

import { computed, onMounted } from 'vue'

/** Simple Warning Visualization. */

registerVisualization('Warnings', 'Any')

type Data = string[]

const props = defineProps<{
  isCircularMenuVisible: boolean
  width: number
  height: number
  fullscreen: boolean
  data: Data | string
}>()
const emit = defineEmits<{
  hide: []
  'update:width': [width: number]
  'update:height': [height: number]
  'update:fullscreen': [fullscreen: boolean]
  'update:preprocessor': [module: string, method: string]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Warnings', 'process_to_json_text')
})

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)
</script>

<template>
  <Visualization
    below-toolbar
    :data="data"
    @hide="emit('hide')"
    :is-circular-menu-visible="isCircularMenuVisible"
    :width="width"
    @update:width="emit('update:width', $event)"
    :height="height"
    @update:height="emit('update:height', $event)"
    :fullscreen="fullscreen"
    @update:fullscreen="emit('update:fullscreen', $event)"
  >
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
