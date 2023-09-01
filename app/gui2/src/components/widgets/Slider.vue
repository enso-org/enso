<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{ modelValue: number; max: number }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: number] }>()

/** The flag in `event.buttons` representing the left mouse button. */
const BUTTON_LEFT_FLAG = 1

function onMouseMove(event: MouseEvent) {
  const currentTarget = event.currentTarget
  if ((event.buttons & BUTTON_LEFT_FLAG) === 0 || !(currentTarget instanceof Element)) { return }
  const rect = currentTarget.getBoundingClientRect()
  const fraction = (event.clientX - rect.left) / (rect.right - rect.left)
  const newValue = Math.round(fraction * props.max)
  emit('update:modelValue', newValue)
}

const inputValue = computed({
  get() {
    return props.modelValue
  },
  set(value) {
    emit('update:modelValue', value)
  }
})
</script>

<template>
  <div class="Slider" @mousemove="onMouseMove">
    <div class="fraction" :style="{ width: `${modelValue * 100 / max}%` }"></div>
    <input type="number" :size="1" class="value" v-model.number="inputValue">
  </div>
</template>

<style scoped>
.Slider {
  clip-path: inset(0 round var(--radius-full));
  position: relative;
  user-select: none;
  display: flex;
  justify-content: space-around;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  width: 56px;
}

.Slider>.fraction {
  position: absolute;
  height: 100%;
  left: 0;
  background: var(--color-widget);
}

.Slider>.value {
  position: relative;
  display: inline-block;
  background: none;
  border: none;
  text-align: center;
  min-width: 0;
  font-weight: 800;
  line-height: 171.5%;
  height: 24px;
  padding-top: 1px;
  padding-bottom: 1px;
  appearance: textfield;
  -moz-appearance: textfield;
}

input::-webkit-outer-spin-button,
input::-webkit-inner-spin-button {
  -webkit-appearance: none;
  margin: 0;
}
</style>
