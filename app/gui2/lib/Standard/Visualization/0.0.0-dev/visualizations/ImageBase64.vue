<script setup lang="ts">
import Visualization from './Visualization.vue'

import { computed } from 'vue'

interface Data {
  mediaType?: string
  base64: string
}

const props = defineProps<{ data: Data | string }>()

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

const DEFAULT_MEDIA_TYPE = 'image/png'
</script>

<template>
  <Visualization :="<any>$attrs">
    <img
      class="Image"
      :src="`data:${data.mediaType ?? DEFAULT_MEDIA_TYPE};base64,${data.base64}`"
    />
  </Visualization>
</template>

<style scoped></style>
