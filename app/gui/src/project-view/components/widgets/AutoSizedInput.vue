<script setup lang="ts">
import { useAutoBlur } from '@/util/autoBlur'
import { getTextWidthByFont } from '@/util/measurement'
import { computed, ref, type StyleValue } from 'vue'

const model = defineModel<string>({ default: '' })
const { placeholder = '' } = defineProps<{
  placeholder?: string | undefined
}>()
const emit = defineEmits<{
  input: [value: string | undefined]
}>()

function onInput() {
  emit('input', model.value)
}

const inputNode = ref<HTMLInputElement>()
useAutoBlur(inputNode)
function onFocus() {
  inputNode.value?.select()
}

const cssFont = computed(() => {
  if (inputNode.value == null) return ''
  const style = window.getComputedStyle(inputNode.value)
  return style.font
})

// Add some extra spacing to allow the text caret to show at the end of input.
const ADDED_WIDTH_PX = 2

const getTextWidth = (text: string) => getTextWidthByFont(text, cssFont.value)
const inputWidth = computed(() => getTextWidth(model.value || placeholder) + ADDED_WIDTH_PX)
const inputStyle = computed<StyleValue>(() => ({ width: `${inputWidth.value}px` }))

function onEnterDown(event: KeyboardEvent) {
  event.stopPropagation()
  inputNode.value?.blur()
}

defineExpose({
  inputWidth,
  getTextWidth,
  select: () => inputNode.value?.select(),
  focus: () => inputNode.value?.focus(),
  blur: () => inputNode.value?.blur(),
  cancel: () => inputNode.value?.blur(),
})
</script>

<template>
  <input
    ref="inputNode"
    v-model="model"
    class="AutoSizedInput input"
    :placeholder="placeholder"
    :style="inputStyle"
    @pointerdown.stop
    @click.stop
    @contextmenu.stop
    @keydown.backspace.stop
    @keydown.delete.stop
    @keydown.arrow-left.stop
    @keydown.arrow-right.stop
    @keydown.enter="onEnterDown"
    @input="onInput"
    @focus="onFocus"
  />
</template>

<style scoped>
.AutoSizedInput {
  position: relative;
  display: inline-block;
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
  &::placeholder {
    font-style: italic;
    color: var(--color-node-text-placeholder);
  }
}

.input::-webkit-outer-spin-button,
.input::-webkit-inner-spin-button {
  -webkit-appearance: none;
  margin: 0;
}
</style>
