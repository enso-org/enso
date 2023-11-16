<script setup lang="ts">
const props = defineProps<{ modelValue: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: boolean] }>()
</script>

<template>
  <div
    class="Checkbox r-24"
    @pointerdown.stop
    @click="emit('update:modelValue', !props.modelValue)"
  >
    <div :class="{ hidden: !props.modelValue }"></div>
  </div>
</template>

<style scoped>
.Checkbox {
  cursor: pointer;
  width: 24px;
  height: 24px;
  padding: 6px;
  border-radius: var(--radius-full);
  background: var(--color-widget);
}

.Checkbox > div {
  width: 12px;
  height: 12px;
  border-radius: var(--radius-full);
  background: var(--color-widget-selected);
}
</style>
