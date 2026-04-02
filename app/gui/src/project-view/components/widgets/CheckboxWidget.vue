<script setup lang="ts">
const props = defineProps<{ modelValue: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: boolean] }>()
</script>

<template>
  <div class="Checkbox clickable" @click.stop="emit('update:modelValue', !props.modelValue)">
    <div class="mark" :class="props.modelValue ? 'checkmark' : 'cross'"></div>
  </div>
</template>

<style scoped>
.Checkbox {
  --widget-checkbox-size: var(--node-port-height);
  --widget-checkbox-check-width: 5px;
  --widget-checkbox-check-height: 9px;
  width: var(--widget-checkbox-size);
  height: var(--widget-checkbox-size);
  border-radius: var(--node-port-border-radius);
  border: 1px solid color-mix(in oklab, var(--color-widget-selected) 40%, transparent);
  background: var(--color-widget);
  display: grid;
  place-items: center;
  transition:
    background-color 0.2s ease,
    border-color 0.2s ease;
}

.Checkbox:hover {
  border-color: color-mix(in oklab, var(--color-widget-selected) 70%, transparent);
}

.selected .Checkbox {
  width: var(--node-port-height);
  height: var(--node-port-height);
  background: color-mix(in oklab, var(--color-widget-selected) 30%, var(--color-node-primary) 70%);
  border-color: color-mix(in oklab, var(--color-widget-selected) 65%, transparent);
}

.mark {
  position: relative;
}

.checkmark {
  width: var(--widget-checkbox-check-width);
  height: var(--widget-checkbox-check-height);
  border-right: 2px solid var(--color-widget-selected);
  border-bottom: 2px solid var(--color-widget-selected);
  transform: translateY(-1px) rotate(45deg);
}

.cross {
  width: 10px;
  height: 10px;
}

.cross::before,
.cross::after {
  content: '';
  position: absolute;
  top: 4px;
  left: 0;
  width: 10px;
  border-top: 2px solid var(--color-widget-selected);
}

.cross::before {
  transform: rotate(45deg);
}

.cross::after {
  transform: rotate(-45deg);
}
</style>
