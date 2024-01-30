<script setup lang="ts">
import { PointerButtonMask, usePointer, useResizeObserver } from '@/composables/events'
import { blurIfNecessary } from '@/util/autoBlur'
import { getTextWidthByFont } from '@/util/measurement'
import { computed, ref, watch, type StyleValue } from 'vue'

const props = defineProps<{
  modelValue: number | string
  limits?: { min: number; max: number } | undefined
}>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: number | string] }>()

const inputFieldActive = ref(false)
// Edited value reflects the `modelValue`, but does not update it until the user defocuses the field.
const editedValue = ref(props.modelValue)
watch(
  () => props.modelValue,
  (newValue) => {
    editedValue.value = newValue
  },
)
const SLIDER_INPUT_THRESHOLD = 4.0

const dragPointer = usePointer(
  (position, event, eventType) => {
    const slider = event.target
    if (!(slider instanceof HTMLElement)) {
      return
    }

    if (eventType === 'stop' && Math.abs(position.relative.x) < SLIDER_INPUT_THRESHOLD) {
      inputNode.value?.focus()
      return
    }

    if (eventType === 'start') {
      event.stopImmediatePropagation()
      return
    }

    if (inputFieldActive.value || props.limits == null) return

    const { min, max } = props.limits
    const rect = slider.getBoundingClientRect()
    const fractionRaw = (position.absolute.x - rect.left) / (rect.right - rect.left)
    const fraction = Math.max(0, Math.min(1, fractionRaw))
    const newValue = min + Math.round(fraction * (max - min))
    editedValue.value = newValue
    if (eventType === 'stop') {
      emit('update:modelValue', editedValue.value)
    }
  },
  PointerButtonMask.Main,
  (event) => !event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey,
)

const sliderWidth = computed(() => {
  if (props.limits == null) return undefined
  if (typeof editedValue.value === 'string') return undefined
  return `${
    ((editedValue.value - props.limits.min) * 100) / (props.limits.max - props.limits.min)
  }%`
})

const inputNode = ref<HTMLInputElement>()
const inputSize = useResizeObserver(inputNode)
const inputMeasurements = computed(() => {
  if (inputNode.value == null) return { availableWidth: 0, font: '' }
  let style = window.getComputedStyle(inputNode.value)
  let availableWidth =
    inputSize.value.x - (parseFloat(style.paddingLeft) + parseFloat(style.paddingRight))
  return { availableWidth, font: style.font }
})

const inputStyle = computed<StyleValue>(() => {
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
    const total = getTextWidthByFont(value, measurements.font)
    const beforeDot = getTextWidthByFont(textBefore, measurements.font)
    const afterDot = getTextWidthByFont(textAfter, measurements.font)
    const blankSpace = Math.max(measurements.availableWidth - total, 0)
    indent = Math.min(Math.max(-blankSpace, afterDot - beforeDot), blankSpace)
  }
  return {
    textIndent: `${indent}px`,
  }
})

function blur() {
  inputFieldActive.value = false
  emit('update:modelValue', editedValue.value)
}

/** To prevent other elements from stealing mouse events (which breaks blur),
 * we instead setup our own `pointerdown` handler while the input is focused.
 * Any click outside of the input field causes `blur`.
 * We don’t want to `useAutoBlur` here, because it would require a separate `pointerdown` handler per input widget.
 * Instead we setup a single handler for the currently focused widget only, and thus safe performance. */
function setupAutoBlur() {
  const options = { capture: true }
  function callback(event: MouseEvent) {
    if (blurIfNecessary(inputNode, event)) {
      window.removeEventListener('pointerdown', callback, options)
    }
  }
  window.addEventListener('pointerdown', callback, options)
}

function focus() {
  inputNode.value?.select()
  inputFieldActive.value = true
  setupAutoBlur()
}
</script>

<template>
  <div
    class="NumericInputWidget"
    v-on="dragPointer.events"
    @keydown.backspace.stop
    @keydown.delete.stop
  >
    <div v-if="props.limits != null" class="fraction" :style="{ width: sliderWidth }"></div>
    <input
      ref="inputNode"
      v-model="editedValue"
      class="value"
      :style="inputStyle"
      @blur="blur"
      @focus="focus"
    />
  </div>
</template>

<style scoped>
.NumericInputWidget {
  position: relative;
  user-select: none;
  justify-content: space-around;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  overflow: clip;
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
  cursor: default;
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
