<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'

const props = defineProps<{ recordMode: boolean }>()
const emit = defineEmits<{ recordOnce: []; 'update:recordMode': [enabled: boolean] }>()
</script>

<template>
  <div class="RecordControl" @pointerdown.stop @pointerup.stop @click.stop>
    <div class="control left-end">
      <ToggleIcon
        icon="record"
        class="button"
        :alt="`${props.recordMode ? 'Enable' : 'Disable'} record mode`"
        :modelValue="props.recordMode"
        @update:modelValue="emit('update:recordMode', $event)"
      />
    </div>
    <div class="control right-end">
      <SvgIcon
        alt="Record once"
        class="button"
        name="record_once"
        draggable="false"
        @click.stop="() => emit('recordOnce')"
      />
    </div>
  </div>
</template>

<style scoped>
.RecordControl {
  user-select: none;
  display: flex;
  place-items: center;
  gap: 1px;
}

.control {
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  padding: 8px 8px;
}

.left-end {
  border-radius: var(--radius-full) 0 0 var(--radius-full);
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
