<script setup lang="ts">
import { usePointer } from '@/composables/events'
import { computed, ref, watch, type CSSProperties, type ComponentInstance } from 'vue'
import { isNumericLiteral } from 'ydoc-shared/ast/tree'
import AutoSizedInput from './AutoSizedInput.vue'

const props = defineProps<{
  modelValue: number | undefined
  placeholder?: string | undefined
  limits?: { min: number; max: number } | undefined
}>()
const emit = defineEmits<{
  'update:modelValue': [modelValue: string | undefined]
  blur: []
  focus: []
  input: [content: string]
}>()

const DEFAULT_PLACEHOLDER = ''
const SLIDER_INPUT_THRESHOLD = 4.0
const MIN_CONTENT_WIDTH = 56

// Edited value reflects the `modelValue`, but does not update it until the user defocuses the field.
const editedValue = ref('')
// Last value which is a parseable number. It's a string, because the Enso number literals differ from js
// representations.
const lastValidValue = ref<string>()
watch(editedValue, (newValue) => {
  if (newValue == '' || isNumericLiteral(newValue)) {
    lastValidValue.value = newValue
  } else if (isNumericLiteral('0' + newValue)) {
    lastValidValue.value = '0' + newValue
  }
})
const valueString = computed(() => (props.modelValue != null ? props.modelValue.toString() : ''))
watch(valueString, (newValue) => (editedValue.value = newValue), { immediate: true })
const inputFieldActive = ref(false)

let dragState: { thresholdMet: boolean } | undefined = undefined
const dragPointer = usePointer(
  (position, event, eventType) => {
    const slider = event.target
    if (!(slider instanceof HTMLElement)) return false

    if (dragState) {
      dragState.thresholdMet ||= Math.abs(position.relative.x) >= SLIDER_INPUT_THRESHOLD
    }

    if (eventType === 'stop' && !dragState?.thresholdMet) {
      inputComponent.value?.focus()
      return
    }

    if (inputFieldActive.value || props.limits == null) return false

    if (eventType === 'start') {
      // Prevent selecting the input's text content.
      event.preventDefault()
      dragState = { thresholdMet: false }
      return
    }

    const { min, max } = props.limits
    const rect = slider.getBoundingClientRect()
    const fractionRaw = (position.absolute.x - rect.left) / (rect.right - rect.left)
    const fraction = Math.max(0, Math.min(1, fractionRaw))
    const newValue = min + Math.round(fraction * (max - min))
    editedValue.value = `${newValue}`
    if (eventType === 'stop') emitUpdate()
  },
  { predicate: (event) => !event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey },
)

const sliderWidth = computed(() => {
  if (props.limits == null) return undefined
  const numberValue = parseFloat(editedValue.value)
  const sliderValue =
    ((numberValue - props.limits.min) * 100) / (props.limits.max - props.limits.min)
  if (isNaN(sliderValue)) return undefined
  return `${sliderValue}%`
})

const inputComponent = ref<ComponentInstance<typeof AutoSizedInput>>()

const inputStyle = computed<CSSProperties>(() => {
  const value = `${editedValue.value}`
  const dotIdx = value.indexOf('.')
  let indent = 0
  if (dotIdx >= 0 && inputComponent.value != null) {
    const { inputWidth, getTextWidth } = inputComponent.value
    const textBefore = value.slice(0, dotIdx)
    const textAfter = value.slice(dotIdx + 1)
    const availableWidth = Math.max(inputWidth, MIN_CONTENT_WIDTH)
    const beforeDot = getTextWidth(textBefore)
    const afterDot = getTextWidth(textAfter)
    const blankSpace = Math.max(availableWidth - inputWidth, 0)
    indent = Math.min(Math.max(-blankSpace, afterDot - beforeDot), blankSpace)
  }
  return {
    textIndent: `${indent}px`,
    // Note: The input element here uses `box-sizing: content-box;`.
    minWidth: `${MIN_CONTENT_WIDTH}px`,
  }
})

function emitUpdate() {
  if (valueString.value !== lastValidValue.value) {
    emit('update:modelValue', lastValidValue.value == '' ? undefined : lastValidValue.value)
  }
}

function blurred() {
  inputFieldActive.value = false
  editedValue.value = lastValidValue.value?.toString() ?? ''
  emit('blur')
  emitUpdate()
}

function focused() {
  inputFieldActive.value = true
  emit('focus')
}

defineExpose({
  cancel: () => {
    editedValue.value = valueString.value
    inputComponent.value?.blur()
  },
  blur: () => inputComponent.value?.blur(),
  focus: () => inputComponent.value?.focus(),
})
</script>

<template>
  <AutoSizedInput
    ref="inputComponent"
    v-model="editedValue"
    autoSelect
    class="NumericInputWidget"
    :class="{ slider: sliderWidth != null }"
    :style="{ ...inputStyle, '--slider-width': sliderWidth }"
    :placeholder="placeholder ?? DEFAULT_PLACEHOLDER"
    v-on="dragPointer.events"
    @click.stop
    @blur="blurred"
    @focus="focused"
    @input="emit('input', editedValue)"
  />
</template>

<style scoped>
.NumericInputWidget {
  position: relative;
  overflow: clip;
  border-radius: var(--radius-full);
  user-select: none;
  padding: 0 4px;
  background: var(--color-widget);
  &:focus {
    background: var(--color-widget-focus);
  }
  &::selection {
    background: var(--color-widget-selection);
  }
}

.NumericInputWidget.slider {
  &:focus {
    /* Color will be blended with background defined below. */
    background-color: var(--color-widget);
  }
  background: linear-gradient(
    to right,
    var(--color-widget-focus) 0 calc(var(--slider-width) - 1px),
    var(--color-widget-slight) calc(var(--slider-width) - 1px) var(--slider-width),
    var(--color-widget) var(--slider-width) 100%
  );
}
</style>
