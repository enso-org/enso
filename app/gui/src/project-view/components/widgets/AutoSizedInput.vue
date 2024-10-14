<script lang="ts">
export type Range = { start: number; end: number }
</script>

<script setup lang="ts">
import { useEvent } from '@/composables/events'
import { useAutoBlur } from '@/util/autoBlur'
import { getTextWidthByFont } from '@/util/measurement'
import { computed, ref, watch, type StyleValue } from 'vue'

const [model, modifiers] = defineModel<string>()
const [selection] = defineModel<Range | undefined>('selection')
const {
  autoSelect = false,
  placeholder = '',
  acceptOnEnter = true,
} = defineProps<{
  autoSelect?: boolean
  placeholder?: string | undefined
  acceptOnEnter?: boolean
}>()
const emit = defineEmits<{
  input: [value: string | undefined]
  change: [value: string | undefined]
}>()

const innerModel = modifiers.lazy ? ref(model.value) : model
if (modifiers.lazy) watch(model, (newVal) => (innerModel.value = newVal))
function onChange() {
  if (modifiers.lazy) model.value = innerModel.value
  emit('change', innerModel.value)
}

function onInput() {
  readInputFieldSelection()
  emit('input', innerModel.value)
}

const inputNode = ref<HTMLInputElement>()
useAutoBlur(inputNode)
function onFocus() {
  if (autoSelect) {
    inputNode.value?.select()
  }
}

const cssFont = computed(() => {
  if (inputNode.value == null) return ''
  const style = window.getComputedStyle(inputNode.value)
  return style.font
})

// Add some extra spacing to allow the text caret to show at the end of input.
const ADDED_WIDTH_PX = 2

const getTextWidth = (text: string) => getTextWidthByFont(text, cssFont.value)
const inputWidth = computed(() => getTextWidth(innerModel.value || placeholder) + ADDED_WIDTH_PX)
const inputStyle = computed<StyleValue>(() => ({ width: `${inputWidth.value}px` }))

function onEnterDown(event: KeyboardEvent) {
  if (acceptOnEnter) {
    event.stopPropagation()
    inputNode.value?.blur()
  }
}

function readInputFieldSelection() {
  if (inputNode.value?.selectionStart != null && inputNode.value.selectionEnd != null) {
    selection.value = {
      start: inputNode.value.selectionStart,
      end: inputNode.value.selectionEnd,
    }
  } else {
    selection.value = undefined
  }
}

// HTMLInputElement's same event is not supported in chrome yet. We just react for any
// selectionchange in the document and check if the input selection changed.
// BUT some operations like deleting does not emit 'selectionChange':
// https://bugs.chromium.org/p/chromium/issues/detail?id=725890
// Therefore we must also refresh selection after changing input.
useEvent(document, 'selectionchange', readInputFieldSelection)

watch(selection, (newPos) => {
  // If boundaries didn't change, don't overwrite selection dir.
  if (
    inputNode.value?.selectionStart !== newPos?.start ||
    inputNode.value?.selectionEnd !== newPos?.end
  ) {
    inputNode.value?.setSelectionRange(newPos?.start ?? null, newPos?.end ?? null)
  }
})

defineExpose({
  inputWidth,
  getTextWidth,
  select: () => inputNode.value?.select(),
  focus: () => inputNode.value?.focus(),
  blur: () => inputNode.value?.blur(),
  cancel: () => {
    innerModel.value = model.value
    inputNode.value?.blur()
  },
})
</script>

<template>
  <input
    ref="inputNode"
    v-model="innerModel"
    class="AutoSizedInput input"
    :placeholder="placeholder"
    :style="inputStyle"
    @pointerdown.stop
    @click.stop
    @keydown.backspace.stop
    @keydown.delete.stop
    @keydown.arrow-left.stop
    @keydown.arrow-right.stop
    @keydown.enter="onEnterDown"
    @input="onInput"
    @change="onChange"
    @focus="onFocus"
  />
</template>

<style scoped>
.AutoSizedInput {
  position: relative;
  display: inline-block;
  background: none;
  border: none;
  text-align: center;
  font-weight: 800;
  line-height: 171.5%;
  height: var(--node-port-height);
  appearance: textfield;
  -moz-appearance: textfield;
  user-select: all;
  box-sizing: content-box;
  &:focus {
    outline: none;
  }
}

.input::-webkit-outer-spin-button,
.input::-webkit-inner-spin-button {
  -webkit-appearance: none;
  margin: 0;
}
</style>
