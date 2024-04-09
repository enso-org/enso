<script lang="ts">
export default {
  name: 'HstJson',
  inheritAttrs: false,
}
</script>

<script lang="ts" setup>
import { defaultKeymap } from '@codemirror/commands'
import { bracketMatching, foldGutter, foldKeymap, indentOnInput } from '@codemirror/language'
import { lintKeymap } from '@codemirror/lint'
import { Compartment } from '@codemirror/state'
import { oneDarkTheme } from '@codemirror/theme-one-dark'
import {
  EditorView,
  ViewUpdate,
  highlightActiveLine,
  highlightActiveLineGutter,
  highlightSpecialChars,
  keymap,
} from '@codemirror/view'
import { onMounted, ref, watch, watchEffect } from 'vue'
import HstWrapper from './HstWrapper.vue'
import { isDark } from './utils'

const props = defineProps<{ title?: string; modelValue: string }>()
const emit = defineEmits<{ 'update:modelValue': [newValue: string] }>()

let editorView: EditorView
const internalValue = ref('')
const editorElement = ref<HTMLInputElement>()

const themes = {
  light: [EditorView.baseTheme({})],
  dark: [oneDarkTheme],
}

const themeConfig = new Compartment()

const extensions = [
  highlightActiveLineGutter(),
  highlightActiveLine(),
  highlightSpecialChars(),
  bracketMatching(),
  indentOnInput(),
  foldGutter(),
  keymap.of([...defaultKeymap, ...foldKeymap, ...lintKeymap]),
  EditorView.updateListener.of((viewUpdate: ViewUpdate) => {
    internalValue.value = viewUpdate.view.state.doc.toString()
  }),
  themeConfig.of(themes.light),
]

onMounted(() => {
  editorView = new EditorView({
    doc: props.modelValue,
    extensions,
    parent: editorElement.value!,
  })

  watchEffect(() => {
    editorView.dispatch({
      effects: [themeConfig.reconfigure(themes[isDark.value ? 'dark' : 'light'])],
    })
  })
})

watch(
  () => props.modelValue,
  (value) => {
    if (internalValue.value === props.modelValue) return
    editorView.dispatch({
      changes: [
        {
          from: 0,
          to: editorView.state.doc.length,
          insert: value,
        },
      ],
    })
  },
)

watch(internalValue, (internalValue) => {
  emit('update:modelValue', internalValue)
})
</script>

<template>
  <HstWrapper
    :title="props.title!"
    class="histoire-code htw-cursor-text"
    :class="$attrs.class"
    :style="$attrs.style"
  >
    <div
      ref="editorElement"
      class="__histoire-code htw-w-full htw-border htw-border-solid htw-border-black/25 dark:htw-border-white/25 focus-within:htw-border-primary-500 dark:focus-within:htw-border-primary-500 htw-rounded-sm htw-box-border htw-overflow-auto htw-resize-y htw-min-h-32 htw-h-48 htw-relative"
      v-bind="{ ...$attrs, class: null!, style: null! }"
    />

    <template #actions>
      <slot name="actions" />
    </template>
  </HstWrapper>
</template>

<style scoped>
.__histoire-code :deep(.cm-editor) {
  height: 100%;
  min-width: 280px;
}
</style>
