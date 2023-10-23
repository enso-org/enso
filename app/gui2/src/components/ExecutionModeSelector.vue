<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { useEvent } from '@/util/events'
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

useEvent(document, 'click', onDocumentClick)
</script>

<template>
  <div ref="executionModeSelectorNode" class="ExecutionModeSelector">
    <div class="execution-mode-button">
      <div class="execution-mode button" @pointerdown.stop="isDropdownOpen = !isDropdownOpen">
        <span v-text="props.modelValue"></span>
      </div>
      <div class="divider"></div>
      <SvgIcon
        name="workflow_play"
        class="play button"
        draggable="false"
        @pointerdown="
          () => {
            isDropdownOpen = false
            emit('execute')
          }
        "
      />
    </div>
    <Transition name="dropdown">
      <div v-if="isDropdownOpen" class="execution-mode-dropdown">
        <template v-for="otherMode in props.modes" :key="otherMode">
          <span
            v-if="modelValue !== otherMode"
            class="button"
            @pointerdown="emit('update:modelValue', otherMode), (isDropdownOpen = false)"
            v-text="otherMode"
          ></span>
        </template>
      </div>
    </Transition>
  </div>
</template>

<style scoped>
span {
  display: inline-block;
  height: 24px;
  padding: 1px 0px;
  vertical-align: middle;
  overflow: clip;
}

.ExecutionModeSelector {
  position: relative;
  color: white;
}

.execution-mode-button {
  display: flex;
  align-items: center;
  height: 24px;
  border-radius: var(--radius-full);
  background: #64b526;

  > .execution-mode {
    font-weight: 600;
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
    color: rgba(255, 255, 255, 0.75);
  }
}

.execution-mode-dropdown {
  display: flex;
  flex-flow: column;
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

.dropdown-enter-active,
.dropdown-leave-active {
  transition: all 0.1s;

  > span {
    transition: all 0.1s;
  }
}

.dropdown-enter-from,
.dropdown-leave-to {
  max-height: 0;
  padding-top: 0;
  padding-bottom: 0;
  overflow: hidden;

  > span {
    padding-top: 0;
    padding-bottom: 0;
    max-height: 0;
  }
}
</style>
