<script setup lang="ts">
import ExecutionModeSelector from '@/components/ExecutionModeSelector.vue'
import type { ExecutionMode } from '@/components/ExecutionModeSelector.vue'

const props = defineProps<{ title: string; mode: ExecutionMode }>()
</script>

<template>
  <div>
    <div class="blur-full"></div>
    <div class="ProjectTitle">
      <span v-text="title" class="title"></span>
      <ExecutionModeSelector :mode="mode" />
    </div>
  </div>
</template>

<style scoped>
.ProjectTitle {
  display: flex;
  align-items: center;
  gap: 8px;
  padding-left: 10px;
  padding-right: 4px;
  padding-top: 4px;
  padding-bottom: 4px;
}

.ProjectTitle>.title {
  height: 20px;
  padding-top: 1px;
  padding-bottom: 1px;
}
</style>
