<script setup lang="ts">
import { PointerButtonMask, usePointer, useResizeObserver } from '@/composables/events'
import { getTextWidth } from '@/util/measurement'
import { computed, ref, type StyleValue } from 'vue'
import { blurIfNecessary } from '@/util/autoBlur'

const props = defineProps<{ 
  modelValue: number;
  limits?: { min: number; max: number } | undefined;
  allowDecimals: boolean;
}>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: number] }>()

const inputFieldActive = ref(false)

const dragPointer = usePointer(
  (position, event, eventType) => {
    const slider = event.target
    if (!(slider instanceof HTMLElement)) {
      return
    }

    if (eventType === 'stop' && position.relative.lengthSquared() < 1.0) { 
      inputNode.value?.focus()
      inputFieldActive.value = true
      return
    }

    if (eventType === 'start') {
      event.stopImmediatePropagation()
      return
    }

    if (inputFieldActive.value == true || props.limits == null) return

    const { min, max } = props.limits
    const rect = slider.getBoundingClientRect()
    const fractionRaw = (position.absolute.x - rect.left) / (rect.right - rect.left)
    const fraction = Math.max(0, Math.min(1, fractionRaw))
    const newValue = min + Math.round(fraction * (max - min))
    emit('update:modelValue', newValue)
  },
  PointerButtonMask.Main,
  (event) => !event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey,
)

const sliderWidth = computed(
  () => props.limits == null ? undefined : `${((props.modelValue - props.limits.min) * 100) / (props.limits.max - props.limits.min)}%`,
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
      emit('update:modelValue', props.allowDecimals ? value : Math.round(value))
    }
  },
})

const disallowedChars = props.allowDecimals ? /[^0-9.-]/g : /[^0-9-]/g
function toNumericOnly(value: string) {
  return value.replace(disallowedChars, '')
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

function blur() {
  inputFieldActive.value = false
  if (inputNode.value != null) inputNode.value.value = `${inputValue.value}`
}

/** To prevent other elements from stealing mouse events (which breaks blur),
  * we instead setup our own `pointerdown` handler while the input is focused.
  * Any click outside of the input field causes `blur`. */
function setupAutoBlur() {
  const options = { capture: true }
  function callback(event: MouseEvent) {
    blurIfNecessary(inputNode, event)
    window.removeEventListener('pointerdown', callback, options)
  }
  window.addEventListener('pointerdown', callback, options)
}
</script>

<template>
  <div class="SliderWidget" v-on="dragPointer.events">
    <div class="fraction" v-if="props.limits != null"  :style="{ width: sliderWidth }"></div>
    <input
      ref="inputNode"
      v-model="inputValue"
      class="value"
      :style="inputStyle"
      @blur="blur"
      @focus="() => inputNode && inputNode.select() || setupAutoBlur()"
    />
  </div>
</template>

<style scoped>
.SliderWidget {
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
