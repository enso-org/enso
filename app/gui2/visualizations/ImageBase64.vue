<script setup lang="ts">
import { registerVisualization } from '@/util/visualizations'

import { computed } from 'vue'

registerVisualization('Image', 'Standard.Image.Data.Image.Image')

const props = defineProps<{
  width: number
  height: number
  data: Data | string
}>()

interface Data {
  mediaType?: string
  base64: string
}

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

const DEFAULT_MEDIA_TYPE = 'image/png'
</script>

<template>
  <img
    class="Image"
    :width="width"
    :height="height"
    :src="`data:${data.mediaType ?? DEFAULT_MEDIA_TYPE};base64,${data.base64}`"
  />
</template>

<style scoped></style>
