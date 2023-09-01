<script setup lang="ts">
import WorkflowPlayIcon from '@/assets/icons/workflow_play.svg'

import { ref } from 'vue'
import { useDocumentEvent } from '../util/events';

const EXECUTION_MODES = ['design', 'live'] as const
export type ExecutionMode = typeof EXECUTION_MODES[number]

export type ExecutionModeSelectorModel = ExecutionMode

const props = defineProps<{ modelValue: ExecutionMode }>()
const emit = defineEmits<{ 'update:modelValue': [mode: ExecutionMode] }>()

const isDropdownOpen = ref(false)

const executionModeSelectorNode = ref<HTMLElement>()
const executionModeDropdownNode = ref<HTMLElement>()

function onDocumentClick(event: MouseEvent) {
  if (
    event.target instanceof Node &&
    executionModeSelectorNode.value?.contains(event.target) === false &&
    executionModeDropdownNode.value?.contains(event.target) === false
  ) {
    isDropdownOpen.value = false
  }
}

useDocumentEvent('click', onDocumentClick)
</script>

<template>
  <div class="ExecutionModeSelectorContainer">
    <div ref="executionModeSelectorNode" class="ExecutionModeSelector">
      <div class="execution-mode button" @click="isDropdownOpen = !isDropdownOpen">
        <span v-text="modelValue"></span>
      </div>
      <div class="divider"></div>
      <img :src="WorkflowPlayIcon" class="play button" draggable="false" />
    </div>
    <div v-if="isDropdownOpen" ref="executionModeDropdownNode" class="ExecutionModeDropdown">
      <template v-for="otherMode in EXECUTION_MODES" :key="otherMode">
        <span v-if="modelValue !== otherMode" v-text="otherMode" class="button"
          @click="emit('update:modelValue', otherMode)"></span>
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
  color: white;
}

.ExecutionModeSelector {
  display: flex;
  border-radius: var(--radius-full);
  background: #64b526;
}

.ExecutionModeSelector>.execution-mode {
  padding-left: 8px;
  padding-right: 7.5px;
}

.ExecutionModeSelector>.divider {
  width: 1px;
  background-color: rgba(0, 0, 0, 0.12);
}

.ExecutionModeSelector>.play {
  padding-left: 7.5px;
  padding-right: 8px;
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
