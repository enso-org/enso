<script setup lang="ts">
const props = defineProps<{ modelValue: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: boolean] }>()
</script>

<template>
  <div class="Checkbox clickable" @click.stop="emit('update:modelValue', !props.modelValue)">
    <div class="track" :class="{ enabled: props.modelValue }">
      <div class="thumb">
        <span class="thumbLabel">{{ props.modelValue ? 'True' : 'False' }}</span>
      </div>
    </div>
  </div>
</template>

<style scoped>
.Checkbox {
  --widget-toggle-width: calc(var(--node-port-height) * 3.6);
  --widget-toggle-height: calc(var(--node-port-height) * 0.9);
  --widget-toggle-padding: 2px;
  width: var(--widget-toggle-width);
  height: var(--widget-toggle-height);
  display: grid;
  place-items: center;
}

.track {
  position: relative;
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
  width: calc(var(--widget-toggle-width) / 2);
  height: calc(var(--widget-toggle-height) - 2 * var(--widget-toggle-padding));
  border-radius: var(--radius-full);
  background: white;
  box-shadow: 0 1px 2px rgb(0 0 0 / 0.25);
  position: relative;
  z-index: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  transition:
    transform 0.2s ease,
    background-color 0.2s ease;
}

.thumbLabel {
  font-size: 9px;
  font-weight: 700;
  line-height: 1;
  letter-spacing: 0.02em;
  color: color-mix(in oklab, var(--color-node-primary) 30%, black 70%);
  user-select: none;
}

.track.enabled .thumb {
  transform: translateX(
    calc(var(--widget-toggle-width) - (var(--widget-toggle-width) / 2) - 4px)
  );
}
</style>
