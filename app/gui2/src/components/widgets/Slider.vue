<script setup lang="ts">
import { ref, computed } from 'vue'
import { useDocumentEvent, useDocumentEventConditional } from '@/util/events'

const props = defineProps<{ modelValue: number; min: number; max: number }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: number] }>()

const sliderNode = ref<HTMLElement>()
const isDragging = ref(false)

/** The flag in `event.buttons` representing the left mouse button. */
const BUTTON_LEFT_FLAG = 1

function onMouseMove(event: MouseEvent) {
  if (isDragging.value && event.currentTarget instanceof HTMLElement) {
    const rect = event.currentTarget.getBoundingClientRect()
    const fraction = (event.clientX - rect.left) / (rect.right - rect.left)
    const newValue = props.min + Math.round(fraction * (props.max - props.min))
    emit('update:modelValue', newValue)
  }
}

useDocumentEvent('mouseup', () => {
  isDragging.value = false
})

useDocumentEventConditional('mousemove', isDragging, (event) => {
  if (sliderNode.value != null) {
    const rect = sliderNode.value.getBoundingClientRect()
    const fractionRaw = (event.clientX - rect.left) / (rect.right - rect.left)
    const fraction = Math.max(0, Math.min(1, fractionRaw))
    const newValue = props.min + Math.round(fraction * (props.max - props.min))
    emit('update:modelValue', newValue)
  }
})

const sliderWidth = computed(
  () => `${((props.modelValue - props.min) * 100) / (props.max - props.min)}%`,
)

const inputValue = computed({
  get() {
    return props.modelValue
  },
  set(value) {
    emit('update:modelValue', value)
  },
})
</script>

<template>
  <div
    ref="sliderNode"
    class="Slider"
    @mousemove="onMouseMove"
    @mousedown="($event.buttons & BUTTON_LEFT_FLAG) !== 0 && (isDragging = true)"
  >
    <div class="fraction" :style="{ width: sliderWidth }"></div>
    <input type="number" :size="1" class="value" v-model.number="inputValue" />
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

.Slider > .fraction {
  position: absolute;
  height: 100%;
  left: 0;
  background: var(--color-widget);
}

.Slider > .value {
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
