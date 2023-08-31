<script setup lang="ts">
import WorkflowPlayIcon from '@/assets/icons/workflow_play.svg'

import { ref } from 'vue'

export type ExecutionMode = 'design' | 'live'

const props = defineProps<{ mode: ExecutionMode }>()

const isDropdownOpen = ref(false)
</script>

<template>
  <div class="ExecutionModeSelector">
    <span v-text="mode" @click="isDropdownOpen = !isDropdownOpen"></span>
    <div class="divider">
      <div></div>
    </div>
    <img :src="WorkflowPlayIcon" class="button" draggable="false" />
  </div>
</template>

<style scoped>
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
</style>
