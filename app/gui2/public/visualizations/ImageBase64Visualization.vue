<script lang="ts">
export const name = 'Image'
export const inputType = 'Standard.Image.Data.Image.Image'

interface Data {
  mediaType?: string
  base64: string
}
</script>

<script setup lang="ts">
import VisualizationContainer from './VisualizationContainer.vue'

import { computed } from 'vue'

const props = defineProps<{ data: Data | string }>()

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

const DEFAULT_MEDIA_TYPE = 'image/png'
</script>

<template>
  <VisualizationContainer :="<any>$attrs" :below-node="true">
    <div class="Image">
      <img :src="`data:${data.mediaType ?? DEFAULT_MEDIA_TYPE};base64,${data.base64}`" />
    </div>
  </VisualizationContainer>
</template>

<style scoped>
.Image {
  > img {
    width: 100%;
  }
}
</style>
