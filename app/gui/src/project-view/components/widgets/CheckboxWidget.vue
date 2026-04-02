<script setup lang="ts">
const props = defineProps<{ modelValue: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: boolean] }>()
</script>

<template>
  <div class="Checkbox clickable" @click.stop="emit('update:modelValue', !props.modelValue)">
    <div class="track" :class="{ enabled: props.modelValue }">
      <div class="thumb"></div>
    </div>
  </div>
</template>

<style scoped>
.Checkbox {
  --widget-toggle-width: calc(var(--node-port-height) * 1.7);
  --widget-toggle-height: calc(var(--node-port-height) * 0.9);
  --widget-toggle-padding: 2px;
  width: var(--widget-toggle-width);
  height: var(--widget-toggle-height);
  display: grid;
  place-items: center;
}

.track {
  width: 100%;
  height: 100%;
  padding: var(--widget-toggle-padding);
  border-radius: var(--radius-full);
  border: 1px solid color-mix(in oklab, var(--color-widget-selected) 35%, transparent);
  background: color-mix(in oklab, var(--color-widget) 85%, black 15%);
  transition:
    background-color 0.2s ease,
    border-color 0.2s ease;
}

.Checkbox:hover .track {
  border-color: color-mix(in oklab, var(--color-widget-selected) 60%, transparent);
}

.selected .track {
  background: color-mix(in oklab, var(--color-widget) 50%, var(--color-node-primary) 50%);
  border-color: color-mix(in oklab, var(--color-widget-selected) 55%, transparent);
}

.track.enabled {
  background: color-mix(in oklab, var(--color-widget-selected) 55%, var(--color-node-primary) 45%);
  border-color: color-mix(in oklab, var(--color-widget-selected) 80%, transparent);
}

.thumb {
  width: calc(var(--widget-toggle-height) - 2 * var(--widget-toggle-padding));
  height: calc(var(--widget-toggle-height) - 2 * var(--widget-toggle-padding));
  border-radius: var(--radius-full);
  background: white;
  box-shadow: 0 1px 2px rgb(0 0 0 / 0.25);
  transition:
    transform 0.2s ease,
    background-color 0.2s ease;
}

.track.enabled .thumb {
  transform: translateX(
    calc(var(--widget-toggle-width) - var(--widget-toggle-height) - 2px)
  );
}
</style>
