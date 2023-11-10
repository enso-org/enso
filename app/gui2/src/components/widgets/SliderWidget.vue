<script setup lang="ts">
import { PointerButtonMask, usePointer, useResizeObserver } from '@/util/events'
import { getTextWidth } from '@/util/measurement'
import { computed, ref } from 'vue'

const props = defineProps<{ modelValue: number; min: number; max: number }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: number] }>()

const dragPointer = usePointer((position, event, eventType) => {
  const slider = event.target
  if (!(slider instanceof HTMLElement)) {
    return
  }

  if (eventType === 'start') {
    event.stopImmediatePropagation()
  }

  const rect = slider.getBoundingClientRect()
  const fractionRaw = (position.absolute.x - rect.left) / (rect.right - rect.left)
  const fraction = Math.max(0, Math.min(1, fractionRaw))
  const newValue = props.min + Math.round(fraction * (props.max - props.min))
  emit('update:modelValue', newValue)
}, PointerButtonMask.Main)

const sliderWidth = computed(
  () => `${((props.modelValue - props.min) * 100) / (props.max - props.min)}%`,
)

const inputValue = computed({
  get() {
    return props.modelValue
  },
  set(value) {
    if (typeof value === 'string') {
      value = parseFloat(toNumericOnly(value))
    }
    if (typeof value === 'number' && !isNaN(value)) {
      emit('update:modelValue', value)
    }
  },
})

function toNumericOnly(value: string) {
  return value.replace(/,/g, '.').replace(/[^0-9.]/g, '')
}

const inputNode = ref<HTMLInputElement>()
const inputSize = useResizeObserver(inputNode)
const inputMeasurements = computed(() => {
  if (inputNode.value == null) return { availableWidth: 0, font: '' }
  let style = window.getComputedStyle(inputNode.value)
  let availableWidth =
    inputSize.value.x - (parseFloat(style.paddingLeft) + parseFloat(style.paddingRight))
  return { availableWidth, font: style.font }
})

const inputStyle = computed(() => {
  if (inputNode.value == null) {
    return {}
  }
  const value = `${props.modelValue}`
  const dotIdx = value.indexOf('.')
  let indent = 0
  if (dotIdx >= 0) {
    const textBefore = value.slice(0, dotIdx)
    const textAfter = value.slice(dotIdx + 1)

    const measurements = inputMeasurements.value
    const total = getTextWidth(value, measurements.font)
    const beforeDot = getTextWidth(textBefore, measurements.font)
    const afterDot = getTextWidth(textAfter, measurements.font)
    const blankSpace = Math.max(measurements.availableWidth - total, 0)
    indent = Math.min(Math.max(-blankSpace, afterDot - beforeDot), blankSpace)
  }
  return {
    textIndent: `${indent}px`,
  }
})

function fixupInputValue() {
  if (inputNode.value != null) inputNode.value.value = `${inputValue.value}`
}
</script>

<template>
  <div class="SliderWidget" v-on="dragPointer.events">
    <div class="fraction" :style="{ width: sliderWidth }"></div>
    <input
      ref="inputNode"
      v-model="inputValue"
      class="value"
      :style="inputStyle"
      @blur="fixupInputValue"
    />
  </div>
</template>

<style scoped>
.SliderWidget {
  clip-path: inset(0 round var(--radius-full));
  position: relative;
  user-select: none;
  justify-content: space-around;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  width: 56px;
}

.fraction {
  position: absolute;
  height: 100%;
  left: 0;
  background: var(--color-widget);
}

.value {
  position: relative;
  display: inline-block;
  background: none;
  border: none;
  text-align: center;
  min-width: 0;
  font-weight: 800;
  line-height: 171.5%;
  height: 24px;
  padding: 0px 4px;
  appearance: textfield;
  -moz-appearance: textfield;
  cursor: none;
}

input {
  width: 100%;
  border-radius: inherit;
  &:focus {
    outline: none;
    background-color: rgba(255, 255, 255, 15%);
  }
}

input::-webkit-outer-spin-button,
input::-webkit-inner-spin-button {
  -webkit-appearance: none;
  margin: 0;
}
</style>
