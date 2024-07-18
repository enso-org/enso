<script setup lang="ts">
import icons from '@/assets/icons.svg'
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgButton from '@/components/SvgButton.vue'
import { ref } from 'vue'
import { TextFormatOptions } from './visualizations/TableVisualization.vue'

const emit = defineEmits<{
  changeFormat: [formatValue: TextFormatOptions]
}>()

const textFormatterSelected = ref(TextFormatOptions.Partial)
watch(textFormatterSelected, (selected) => emit('changeFormat', selected))

const iconPath = `${icons}#paragraph`

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
      <button
        :class="`${textFormatterSelected === TextFormatOptions.On && 'selected'}`"
        :onclick="() => setTextFormatterSelected(TextFormatOptions.On)"
        title="Text displayed in monaspace font and all whitespace characters displayed as a symbol"
      >
        <svg viewBox="0 0 16 16" width="16" height="16" style="stroke: black; fill: #000000">
          <use :href="`${iconPath}`" />
        </svg>
        <div>All Whitespace rendering</div>
      </button>

      <button
        :onclick="() => setTextFormatterSelected(TextFormatOptions.Partial)"
        :class="`${textFormatterSelected === TextFormatOptions.Partial && 'selected'}`"
        title="Text displayed in monaspace font, only multiple spaces displayed with &#183;"
      >
        <svg viewBox="0 0 16 16" width="16" height="16" style="stroke: grey; fill: #808080">
          <use :href="`${iconPath}`" />
        </svg>
        <div>Partial whitespace rendering</div>
      </button>

      <button
        :onclick="() => setTextFormatterSelected(TextFormatOptions.Off)"
        :class="`${textFormatterSelected === TextFormatOptions.Off && 'selected'}`"
        title="No formatting applied to text"
      >
        <div class="strikethrough">
          <svg viewBox="0 0 16 16" width="16" height="16">
            <use :href="`${iconPath}`" />
          </svg>
        </div>

        <div>No whitespace rendering</div>
      </button>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.TextFormattingSelector {
  background: var(--color-frame-bg);
  border-radius: 16px;
}

:deep(.DropdownMenuContent) {
  margin-top: 2px;
  padding: 4px;

  > * {
    display: flex;
    align-items: center;
    padding-left: 8px;
    padding-right: 8px;
  }
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

.strikethrough {
  position: relative;
}
.strikethrough:before {
  position: absolute;
  content: '';
  left: 0;
  top: 50%;
  right: 0;
  border-top: 1px solid;
  border-color: black;

  -webkit-transform: rotate(-20deg);
  -moz-transform: rotate(-20deg);
  -ms-transform: rotate(-20deg);
  -o-transform: rotate(-20deg);
  transform: rotate(-20deg);
}
</style>
