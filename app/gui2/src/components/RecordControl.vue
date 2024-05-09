<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'

const props = defineProps<{ recordMode: boolean }>()
const emit = defineEmits<{ recordOnce: []; 'update:recordMode': [enabled: boolean] }>()
</script>

<template>
  <div class="RecordControl">
    <div class="control left-end" @click.stop="() => emit('update:recordMode', !props.recordMode)">
      <ToggleIcon
        icon="record"
        class="button"
        :alt="`${props.recordMode ? 'Enable' : 'Disable'} record mode`"
        :modelValue="props.recordMode"
        @update:modelValue="emit('update:recordMode', $event)"
      />
    </div>
    <div class="control right-end" @click.stop="() => emit('recordOnce')">
      <SvgIcon alt="Record once" class="button" name="record_once" draggable="false" :scale="1.5" />
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
  width: 42px;
  cursor: pointer;
}

.left-end {
  border-radius: var(--radius-full) 0 0 var(--radius-full);

  .button {
    margin: 0 4px 0 auto;
  }
}

.right-end {
  border-radius: 0 var(--radius-full) var(--radius-full) 0;

  .button {
    position: relative;
    top: -4px;
    margin: 0 auto 0 0;
  }
}

.toggledOn {
  color: #ba4c40;
}

.button:active {
  color: #ba4c40;
}
</style>
