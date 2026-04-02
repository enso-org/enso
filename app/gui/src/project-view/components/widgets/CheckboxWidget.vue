<script setup lang="ts">
const props = defineProps<{ modelValue: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: boolean] }>()
</script>

<template>
  <div class="Checkbox clickable" @click.stop="emit('update:modelValue', !props.modelValue)">
    <span class="label" :class="{ enabled: props.modelValue }">
      {{ props.modelValue ? 'True' : 'False' }}
    </span>
  </div>
</template>

<style scoped>
.Checkbox {
  display: grid;
  place-items: center;
}

.label {
  min-width: calc(var(--node-port-height) * 2.2);
  height: calc(var(--node-port-height) * 0.9);
  padding: 0 10px;
  border-radius: var(--radius-full);
  border: 1px solid color-mix(in oklab, var(--color-widget-selected) 35%, transparent);
  background: color-mix(in oklab, var(--color-widget) 85%, black 15%);
  color: color-mix(in oklab, white 88%, transparent);
  font-size: 10px;
  font-weight: 700;
  line-height: 1;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  transition:
    background-color 0.2s ease,
    border-color 0.2s ease,
    color 0.2s ease;
}

.Checkbox:hover .label {
  border-color: color-mix(in oklab, var(--color-widget-selected) 60%, transparent);
}

.selected .label {
  background: color-mix(in oklab, var(--color-widget) 50%, var(--color-node-primary) 50%);
  border-color: color-mix(in oklab, var(--color-widget-selected) 55%, transparent);
}

.label.enabled {
  background: color-mix(in oklab, var(--color-widget-selected) 55%, var(--color-node-primary) 45%);
  border-color: color-mix(in oklab, var(--color-widget-selected) 80%, transparent);
  color: white;
}
</style>
