<script setup lang="ts">
import ExecutionModeSelector from '@/components/ExecutionModeSelector.vue'
import type { ExecutionMode } from '@/components/ExecutionModeSelector.vue'

export interface ProjectTitleModel {
  mode: ExecutionMode
}

const props = defineProps<{ title: string; modelValue: ProjectTitleModel }>()
const emits = defineEmits<{ 'update:modelValue': [model: ProjectTitleModel] }>()
</script>

<template>
  <div>
    <div class="blur-full"></div>
    <div class="ProjectTitle">
      <span v-text="title" class="title"></span>
      <ExecutionModeSelector v-model="modelValue.mode" />
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
  display: inline-block;
  height: 20px;
  padding-top: 1px;
  padding-bottom: 1px;
}
</style>
