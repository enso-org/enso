<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { computed } from 'vue'

const props = defineProps<{ mode: string }>()
const emit = defineEmits<{ execute: []; 'update:mode': [mode: string] }>()

const recordMode = computed(() => props.mode === 'live')
</script>

<template>
  <div class="RecordControl">
    <div class="control left-end">
      <ToggleIcon
        icon="record"
        :alt="`${recordMode ? 'Enable' : 'Disable'} record mode`"
        :modelValue="recordMode"
        @update:modelValue="emit('update:mode', $event ? 'live' : 'design')"
      />
    </div>
    <div class="control right-end">
      <SvgIcon
        alt="Record once"
        class="button"
        name="record_once"
        draggable="false"
        :width="24"
        @pointerdown="() => emit('execute')"
      />
    </div>
  </div>
</template>

<style scoped>
.RecordControl {
  user-select: none;
  display: flex;
  place-items: center;
  backdrop-filter: var(--blur-app-bg);
  gap: 1px;
}

.control {
  background: var(--color-frame-bg);
  padding: 8px 8px;
}

.left-end {
  border-radius: var(--radius-full) 0 0 var(--radius-full);
  padding-right: 8px;
}

.right-end {
  border-radius: 0 var(--radius-full) var(--radius-full) 0;
}

.toggledOn {
  color: #ba4c40;
}

.button:active {
  color: #ba4c40;
}
</style>
