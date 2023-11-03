<script setup lang="ts">
  import { computed } from 'vue'
  import type { UploadingFile } from '@/stores/graph'
  import { Vec2 } from '@/util/vec2'
  const props = defineProps<{
    file: UploadingFile,
  }>()

const transform = computed(() => {
  let pos = props.file.position
  return `translate(${pos.x}px, ${pos.y}px)`
})
</script>

<template>
  <div class="UploadingFile" :style="{ transform, 'background-position': `${200 - props.file.percentage}% 0`, }">
    <span>{{ `Uploading ${props.file.name} (${props.file.percentage}%)` }}</span>
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
  transition: outline 0.2s ease;
  outline: 0px solid transparent;
  background: linear-gradient(to right, #e0e0e0 0%, #e0e0e0 50%, #ffffff 50%, #ffffff 100%);
  background-size: 200% 100%;
}
</style>
