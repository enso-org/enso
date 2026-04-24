<script setup lang="ts">
import { useProgressBackground } from '@/composables/progressBar'
import type { UploadingFile } from '@/stores/awareness'
import { computed } from 'vue'

const { name, file } = defineProps<{
  name: string
  file: UploadingFile
}>()

const transform = computed(() => {
  const pos = file.position
  return `translate(${pos.x}px, ${pos.y}px)`
})

const { progressStyles } = useProgressBackground(() => file.sizePercentage)
</script>

<template>
  <div class="UploadingFile" :style="{ transform, ...progressStyles }">
    {{ `Uploading ${name} (${file.sizePercentage}%)` }}
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
  --progress-background-initial: #e0e0e0;
  --progress-background-final: #ffffff;
}
</style>
