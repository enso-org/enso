<script setup lang="ts">
import { PointerButtonMask, usePointer } from '@/composables/events'
import { computed, ref, watch, type ComponentInstance, type StyleValue } from 'vue'
import AutoSizedInput from './AutoSizedInput.vue'

const props = defineProps<{
  modelValue: number | string
  limits?: { min: number; max: number } | undefined
}>()
const emit = defineEmits<{
  'update:modelValue': [modelValue: number | string]
  blur: []
  focus: []
  input: [content: string]
}>()

const inputFieldActive = ref(false)
// Edited value reflects the `modelValue`, but does not update it until the user defocuses the field.
const editedValue = ref(`${props.modelValue}`)
watch(
  () => props.modelValue,
  (newValue) => {
    editedValue.value = `${newValue}`
  },
)
const SLIDER_INPUT_THRESHOLD = 4.0

const dragPointer = usePointer(
  (position, event, eventType) => {
    const slider = event.target
    if (!(slider instanceof HTMLElement)) return false

    if (eventType === 'stop' && Math.abs(position.relative.x) < SLIDER_INPUT_THRESHOLD) {
      inputComponent.value?.focus()
      return
    }

    if (eventType === 'start') {
      event.stopImmediatePropagation()
      return
    }

    if (inputFieldActive.value || props.limits == null) return false

    const { min, max } = props.limits
    const rect = slider.getBoundingClientRect()
    const fractionRaw = (position.absolute.x - rect.left) / (rect.right - rect.left)
    const fraction = Math.max(0, Math.min(1, fractionRaw))
    const newValue = min + Math.round(fraction * (max - min))
    editedValue.value = `${newValue}`
    if (eventType === 'stop') emitUpdate()
  },
  PointerButtonMask.Main,
  (event) => !event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey,
)

const sliderWidth = computed(() => {
  if (props.limits == null) return undefined
  const numberValue = parseFloat(editedValue.value)
  if (isNaN(numberValue)) return undefined
  return `${
    ((numberValue - props.limits.min) * 100) / (props.limits.max - props.limits.min)
  }%`
})

const inputComponent = ref<ComponentInstance<typeof AutoSizedInput>>()
const MIN_CONTENT_WIDTH = 56

const inputStyle = computed<StyleValue>(() => {
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
  if (`${props.modelValue}` !== editedValue.value) {
    emit('update:modelValue', editedValue.value)
  }
}

function blurred() {
  inputFieldActive.value = false
  emit('blur')
  emitUpdate()
}

function focused() {
  inputFieldActive.value = true
  emit('focus')
}

defineExpose({
  cancel: () => {
    editedValue.value = `${props.modelValue}`
    inputComponent.value?.blur()
  },
  blur: () => inputComponent.value?.blur(),
  focus: () => inputComponent.value?.focus(),
})
</script>

<template>
  <label class="NumericInputWidget">
    <div v-if="props.limits != null" class="slider" :style="{ width: sliderWidth }"></div>
    <AutoSizedInput
      ref="inputComponent"
      v-model="editedValue"
      autoSelect
      :style="inputStyle"
      v-on="dragPointer.events"
      @blur="blurred"
      @focus="focused"
      @input="emit('input', editedValue)"
    />
  </label>
</template>

<style scoped>
.NumericInputWidget {
  position: relative;
  overflow: clip;
  border-radius: var(--radius-full);
}
.AutoSizedInput {
  user-select: none;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  overflow: clip;
  padding: 0px 4px;
  &:focus {
    background: var(--color-widget-focus);
  }
}

.slider {
  position: absolute;
  height: 100%;
  left: 0;
  background: var(--color-widget);
}
</style>
