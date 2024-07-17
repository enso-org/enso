<script setup lang="ts">
import icons from '@/assets/icons.svg'
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgButton from '@/components/SvgButton.vue'
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

const open = ref(false)

const toggleMenu = () => {
  open.value = !open.value
}
</script>

<template>
  <DropdownMenu v-model:open="open" class="TextFormattingSelector">
    <template #button
      ><SvgButton name="paragraph" title="Text Display Options" @click.stop="toggleMenu()" />
    </template>

    <template #entries>
      <div class="button-list-wrapper">
        <button
          :class="`${textFormatterSelected === TextFormatOptions.On && 'selected'}`"
          :onclick="() => setTextFormatterSelected(TextFormatOptions.On)"
          title="Text displayed in monaspace font and all whitespace characters displayed as a symbol"
        >
          <svg viewBox="0 0 16 16" width="16" height="16" style="stroke: black; fill: #000000">
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
  </DropdownMenu>
</template>

<style scoped>
.TextFormattingSelector {
  background: var(--color-frame-bg);
  border-radius: var(--radius-full);
  margin: 0 12px 0 auto;
  width: inherit;
}

:deep(.DropdownMenuContent) {
  width: 250px;
  margin-top: 2px;
  padding: 4px;

  > * {
    display: flex;
    align-items: center;
    padding-left: 8px;
    padding-right: 8px;
  }
}

.button-list-wrapper {
  position: absolute;
  display: flex;
  flex-direction: column;
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
