<script setup lang="ts">
const props = defineProps<{ modelValue: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: boolean] }>()
</script>

<template>
  <div class="Checkbox clickable" @click.stop="emit('update:modelValue', !props.modelValue)">
    <div :class="{ hidden: !props.modelValue }"></div>
  </div>
</template>

<style scoped>
.Checkbox {
  --widget-checkbox-inner-size: 12px;
  width: var(--node-port-height);
  height: var(--node-port-height);
  padding: calc((var(--node-port-height) - var(--widget-checkbox-inner-size)) / 2);
  border-radius: var(--node-port-border-radius);
  background: var(--color-widget);
}

.Checkbox > div {
  width: var(--widget-checkbox-inner-size);
  height: var(--widget-checkbox-inner-size);
  border-radius: var(--radius-full);
  background: var(--color-widget-selected);
}

.hidden {
  display: none;
}
</style>
