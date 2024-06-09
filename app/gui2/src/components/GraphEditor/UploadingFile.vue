<script setup lang="ts">
import type { UploadingFile } from '@/stores/awareness'
import { computed } from 'vue'

const props = defineProps<{
  name: string
  file: UploadingFile
}>()

const transform = computed(() => {
  let pos = props.file.position
  return `translate(${pos.x}px, ${pos.y}px)`
})

const backgroundOffset = computed(() => 200 - props.file.sizePercentage)
</script>

<template>
  <div
    class="UploadingFile"
    :style="{ transform, 'background-position': `${backgroundOffset}% 0` }"
  >
    <span>{{ `Uploading ${props.name} (${props.file.sizePercentage}%)` }}</span>
  </div>
</template>

<style scoped>
.UploadingFile {
  position: absolute;
  height: 32px;
  border-radius: 16px;
  display: flex;
  flex-direction: row;
  align-items: center;
  white-space: nowrap;
  padding: 4px 8px;
  z-index: 2;
  outline: 0px solid transparent;
  background: linear-gradient(to right, #e0e0e0 0%, #e0e0e0 50%, #ffffff 50%, #ffffff 100%);
  background-size: 200% 100%;
}
</style>
