<script setup lang="ts">
import { useResizeObserver } from '@/composables/events'
import { escape } from '@/util/ast/text'
import { useAutoBlur } from '@/util/autoBlur'
import { getTextWidthByFont } from '@/util/measurement'
import { computed, ref, watch, type StyleValue } from 'vue'

const props = defineProps<{
  modelValue: string
}>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: string] }>()

// Edited value reflects the `modelValue`, but does not update it until the user defocuses the field.
const editedValue = ref(props.modelValue)
watch(
  () => props.modelValue,
  (newValue) => {
    editedValue.value = newValue
  },
)

const inputNode = ref<HTMLInputElement>()
useAutoBlur(inputNode)
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
  const value = `${editedValue.value}`
  const measurements = inputMeasurements.value
  const width = getTextWidthByFont(value, measurements.font)
  return {
    width: `${width}px`,
  }
})

function blur() {
  emit('update:modelValue', `'${escape(editedValue.value)}'`)
}
</script>

<template>
  <div
    class="EnsoTextInputWidget"
    @pointerdown.stop="() => inputNode?.focus()"
    @keydown.backspace.stop
    @keydown.delete.stop
  >
    <input
      ref="inputNode"
      v-model="editedValue"
      class="value"
      :style="inputStyle"
      @keydown.enter.stop="($event.target as HTMLInputElement).blur()"
      @blur="blur"
    />
  </div>
</template>

<style scoped>
.EnsoTextInputWidget {
  position: relative;
  user-select: none;
  background: var(--color-widget);
  border-radius: var(--radius-full);
}

.value {
  position: relative;
  display: inline-block;
  background: none;
  border: none;
  min-width: 24px;
  text-align: center;
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
