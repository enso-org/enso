<script setup lang="ts">
import icons from '@/assets/icons.svg'
import { ref } from 'vue'
import { TextFormatOptions } from './visualizations/TableVisualization.vue'

const emit = defineEmits<{
  changeFormat: [formatValue: TextFormatOptions]
}>()

const textFormatterSelected = ref(TextFormatOptions.Special)
const setTextFormatterSelected = (option: TextFormatOptions) => {
  textFormatterSelected.value = option
  emit('changeFormat', textFormatterSelected.value)
}

const iconPathOn = `${icons}#paragraph`
const iconPathOff = `${icons}#text2`
</script>

<template>
  <div class="TextFormattingSelector">
    <button
      :class="`${textFormatterSelected === TextFormatOptions.On && 'selected'}`"
      :onclick="() => setTextFormatterSelected(TextFormatOptions.On)"
      title="Text displayed in monaspace font and all whitespace characters displayed as a symbol"
    >
      <svg viewBox="0 0 16 16" width="16" height="16" style="stroke: black; fill: #808080">
        <use :href="`${iconPathOn}`" />
      </svg>
    </button>

    <button
      :onclick="() => setTextFormatterSelected(TextFormatOptions.Special)"
      :class="`${textFormatterSelected === TextFormatOptions.Special && 'selected'}`"
      title="Text displayed in monaspace font, only multiple spaces displayed with &#183;"
    >
      <svg viewBox="0 0 16 16" width="16" height="16" style="stroke: grey; fill: #808080">
        <use :href="`${iconPathOn}`" />
      </svg>
    </button>

    <button
      :onclick="() => setTextFormatterSelected(TextFormatOptions.Off)"
      :class="`${textFormatterSelected === TextFormatOptions.Off && 'selected'}`"
      title="No formatting applied to text"
    >
      <svg viewBox="0 0 16 16" width="16" height="16">
        <use :href="`${iconPathOff}`" />
      </svg>
    </button>
  </div>
</template>

<style scoped>
.TextFormattingSelector {
  height: 100%;
  border-radius: 16px;
  display: flex;
  flex-direction: row;
  width: inherit;
}

button {
  width: 100%;
  height: 100%;
  display: flex;
  gap: 4px;
  align-items: center;
  padding: 0 8px;
  border-radius: 12px;
  white-space: nowrap;

  &.selected {
    background: var(--color-menu-entry-selected-bg);
  }

  &:hover {
    background: var(--color-menu-entry-hover-bg);
  }

  &:active {
    background-color: var(--color-menu-entry-active-bg);
  }
}
</style>
