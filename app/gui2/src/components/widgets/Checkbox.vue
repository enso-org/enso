<script setup lang="ts">
const props = defineProps<{ modelValue: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: boolean] }>()
</script>

<template>
  <div class="Checkbox" @click="emit('update:modelValue', !modelValue)">
    <div :class="{ hidden: !modelValue }"></div>
  </div>
</template>

<style scoped>
.Checkbox {
  cursor: pointer;
  width: 24px;
  height: 24px;
  border-radius: var(--radius-full);
  background: var(--color-widget);
}

.Checkbox > div {
  margin: 6px;
  width: 12px;
  height: 12px;
  border-radius: var(--radius-full);
  background: var(--color-widget-selected);
}
</style>
