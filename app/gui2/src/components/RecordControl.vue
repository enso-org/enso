<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'

const props = defineProps<{ recordMode: boolean }>()
const emit = defineEmits<{ recordOnce: []; 'update:recordMode': [enabled: boolean] }>()
</script>

<template>
  <div class="RecordControl">
    <div class="control left-end">
      <ToggleIcon
        icon="record"
        class="iconButton"
        title="Record"
        :modelValue="props.recordMode"
        @update:modelValue="emit('update:recordMode', $event)"
      />
    </div>
    <div class="control right-end">
      <SvgButton
        title="Record Once"
        class="iconButton"
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
  padding: 4px 4px;
  width: 42px;
}

.left-end {
  border-radius: var(--radius-full) 0 0 var(--radius-full);

  .iconButton {
    margin: 0 0 0 auto;
  }
}

.right-end {
  border-radius: 0 var(--radius-full) var(--radius-full) 0;

  .iconButton {
    position: relative;
    margin: 0 auto 0 0;
    --icon-width: 24px;
  }
}

.toggledOn {
  color: #ba4c40;
}

.iconButton:active {
  color: #ba4c40;
}
</style>
