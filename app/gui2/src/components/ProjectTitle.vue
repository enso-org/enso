<script setup lang="ts">
import ExecutionModeSelector from '@/components/ExecutionModeSelector.vue'

const props = defineProps<{ title: string; modes: string[]; mode: string }>()
const emit = defineEmits<{ execute: []; 'update:mode': [mode: string] }>()
</script>

<template>
  <div class="ProjectTitle">
    <span class="title" v-text="title"></span>
    <ExecutionModeSelector
      :modes="modes"
      :model-value="mode"
      @execute="emit('execute')"
      @update:modelValue="emit('update:mode', $event)"
    />
  </div>
</template>

<style scoped>
.ProjectTitle {
  user-select: none;
  display: flex;
  align-items: center;
  background: var(--color-frame-bg);
  border-radius: var(--radius-full);
  gap: 8px;
  padding-left: 10px;
  padding-right: 4px;
  padding-top: 4px;
  padding-bottom: 4px;

  > .title {
    display: inline-block;
    height: 20px;
    padding-top: 1px;
    padding-bottom: 1px;
  }
}
</style>
