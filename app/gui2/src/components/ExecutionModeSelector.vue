<script setup lang="ts">
import WorkflowPlayIcon from '@/assets/icons/workflow_play.svg'

import { onMounted, onUnmounted, ref } from 'vue'

const EXECUTION_MODES = ['design', 'live'] as const
export type ExecutionMode = typeof EXECUTION_MODES[number]

export type ExecutionModeSelectorModel = ExecutionMode

defineProps<{ modelValue: ExecutionMode }>()
const emit = defineEmits<{ 'update:modelValue': [mode: ExecutionMode] }>()

const isDropdownOpen = ref(false)

function onDocumentClick() {
  isDropdownOpen.value = false
}

onMounted(() => {
  document.addEventListener('click', onDocumentClick)
})

onUnmounted(() => {
  document.removeEventListener('click', onDocumentClick)
})
</script>

<template>
  <div class="ExecutionModeSelectorContainer">
    <div class="ExecutionModeSelector">
      <div>
        <span v-text="modelValue" @click="$event.stopPropagation(); isDropdownOpen = !isDropdownOpen"
          class="button"></span>
      </div>
      <div class="divider">
        <div></div>
      </div>
      <img :src="WorkflowPlayIcon" class="button" draggable="false" />
    </div>
    <div v-if="isDropdownOpen" class="ExecutionModeDropdown">
      <template v-for="otherMode in EXECUTION_MODES">
        <span v-if="modelValue !== otherMode" v-text="otherMode" class="button"
          @click="$event.stopPropagation(); emit('update:modelValue', otherMode)"></span>
      </template>
    </div>
  </div>
</template>

<style scoped>
span {
  display: inline-block;
  height: 20px;
  padding-top: 1px;
  padding-bottom: 1px;
}

.ExecutionModeSelectorContainer {
  position: relative;
}

.ExecutionModeSelector {
  display: flex;
  border-radius: var(--radius-full);
  background: #64b526;
  padding-left: 8px;
  padding-right: 8px;
}

.ExecutionModeSelector>.divider {
  display: flex;
  width: 16px;
  justify-content: space-around;
}

.ExecutionModeSelector>.divider>div {
  width: 1px;
  background-color: rgba(0, 0, 0, 0.12);
}

.ExecutionModeDropdown {
  position: absolute;
  background: #64b526;
  width: 100%;
  top: calc(100% + 8px);
  border-radius: 10px;
  padding: 4px;
}

.ExecutionModeDropdown>span {
  border-radius: 6px;
  padding-left: 4px;
  padding-right: 4px;
  width: 100%;
}

.ExecutionModeDropdown>span:hover {
  background: var(--color-dim);
}
</style>
