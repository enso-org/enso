<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'

import { useDocumentEvent } from '@/util/events'

import { ref } from 'vue'

const props = defineProps<{ modes: string[]; modelValue: string }>()
const emit = defineEmits<{ execute: []; 'update:modelValue': [mode: string] }>()

const isDropdownOpen = ref(false)

const executionModeSelectorNode = ref<HTMLElement>()

function onDocumentClick(event: MouseEvent) {
  if (
    event.target instanceof Node &&
    executionModeSelectorNode.value?.contains(event.target) === false
  ) {
    isDropdownOpen.value = false
  }
}

useDocumentEvent('click', onDocumentClick)
</script>

<template>
  <div ref="executionModeSelectorNode" class="ExecutionModeSelector">
    <div class="execution-mode-button">
      <div class="execution-mode button" @click="isDropdownOpen = !isDropdownOpen">
        <span v-text="modelValue"></span>
      </div>
      <div class="divider"></div>
      <SvgIcon
        name="workflow_play"
        class="play button"
        draggable="false"
        @click="
          () => {
            isDropdownOpen = false
            emit('execute')
          }
        "
      />
    </div>
    <div v-if="isDropdownOpen" class="execution-mode-dropdown">
      <template v-for="otherMode in modes" :key="otherMode">
        <span
          v-if="modelValue !== otherMode"
          v-text="otherMode"
          class="button"
          @click="emit('update:modelValue', otherMode)"
        ></span>
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

.ExecutionModeSelector {
  position: relative;
  color: white;
}

.execution-mode-button {
  display: flex;
  align-items: center;
  border-radius: var(--radius-full);
  background: #64b526;

  > .execution-mode {
    padding: 0 8px;
  }

  > .divider {
    align-self: stretch;
    width: 1px;
    background-color: rgba(0, 0, 0, 0.12);
  }

  > .play {
    box-sizing: content-box;
    padding: 0 8px;
  }
}

.execution-mode-dropdown {
  position: absolute;
  background: #64b526;
  border-radius: 10px;
  width: 100%;
  top: 100%;
  margin-top: 4px;
  padding: 4px;

  > span {
    border-radius: 6px;
    padding-left: 4px;
    padding-right: 4px;
    width: 100%;
  }

  *:hover {
    background: var(--color-dim);
  }
}
</style>
