<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'
import Visualization from './Visualization.vue'

import { computed } from 'vue'

registerVisualization('Image (Base64)', 'Standard.Image.Data.Image.Image')

interface Data {
  mediaType?: string
  base64: string
}

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
}>()

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

const DEFAULT_MEDIA_TYPE = 'image/png'
</script>

<template>
  <Visualization
    @hide="emit('hide')"
    :is-circular-menu-visible="isCircularMenuVisible"
    :width="width"
    @update:width="emit('update:width', $event)"
    :height="height"
    @update:height="emit('update:height', $event)"
    :fullscreen="fullscreen"
    @update:fullscreen="emit('update:fullscreen', $event)"
  >
    <img
      class="Image"
      :src="`data:${data.mediaType ?? DEFAULT_MEDIA_TYPE};base64,${data.base64}`"
    />
  </Visualization>
</template>

<style scoped></style>
