<script setup lang="ts">
import { useResizeObserver } from '@/composables/events'
import { escape, unescape } from '@/util/ast/abstract'
import { blurIfNecessary } from '@/util/autoBlur'
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

const separators = /(^('''|"""|['"]))|(('''|"""|['"])$)/g
/** Display the value in a more human-readable form for easier editing. */
function prepareForEditing() {
  editedValue.value = unescape(editedValue.value.replace(separators, ''))
}

function focus() {
  setupAutoBlur()
  prepareForEditing()
}

const escapedValue = computed(() => `'${escape(editedValue.value)}'`)

function blur() {
  emit('update:modelValue', escapedValue.value)
  editedValue.value = props.modelValue
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
      @focus="focus"
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
