<script setup lang="ts">
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
</script>

<template>
  <div class="TextFormattingSelector">
    <button
      :onclick="() => setTextFormatterSelected(TextFormatOptions.Special)"
      :selected="textFormatterSelected === TextFormatOptions.Special"
    >
      Special
    </button>
    <button
      :active="textFormatterSelected === TextFormatOptions.Off"
      :onclick="() => setTextFormatterSelected(TextFormatOptions.Off)"
      :selected="textFormatterSelected === TextFormatOptions.Off"
    >
      Off
    </button>
    <button
      :active="textFormatterSelected === TextFormatOptions.On"
      :onclick="() => setTextFormatterSelected(TextFormatOptions.On)"
      :selected="textFormatterSelected === TextFormatOptions.On"
    >
      On
    </button>
  </div>
</template>

<style scoped>
.TextFormattingSelector {
  z-index: 6;
  border-radius: 16px;
  top: 100%;
  display: flex;
  flex-direction: row;

  &:before {
    content: '';
    width: 100%;
    height: 100%;
    border-radius: 16px;
    background: var(--color-app-bg);
    backdrop-filter: var(--blur-app-bg);
  }
}

button {
  width: 100%;
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
