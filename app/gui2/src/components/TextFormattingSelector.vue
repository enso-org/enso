<script setup lang="ts">
import icons from '@/assets/icons.svg'
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { ref, watch } from 'vue'
import { TextFormatOptions } from './visualizations/TableVisualization.vue'

const emit = defineEmits<{
  changeFormat: [formatValue: TextFormatOptions]
}>()

const textFormatterSelected = ref(TextFormatOptions.Partial)
watch(textFormatterSelected, (selected) => emit('changeFormat', selected))

const open = ref(false)
</script>

<template>
  <DropdownMenu v-model:open="open" class="TextFormattingSelector">
    <template #button><SvgIcon name="paragraph" title="Text Display Options" /> </template>

    <template #entries>
      <SvgButton
        name="paragraph"
        class="full-format"
        label="Full whitespace rendering"
        :title="`Text displayed in monaspace font and all whitespace characters displayed as symbols`"
        @click.stop="() => emit('changeFormat', TextFormatOptions.On)"
      />

      <SvgButton
        name="paragraph"
        class="partial"
        label="Partial whitespace rendering"
        :title="`Text displayed in monaspace font, only multiple spaces displayed with &#183;`"
        @click.stop="() => emit('changeFormat', TextFormatOptions.Partial)"
      />
      <button
        class="OffButton"
        title="`No formatting applied to text`"
        @click.stop="() => emit('changeFormat', TextFormatOptions.Off)"
      >
        <div class="strikethrough">
          <SvgIcon name="paragraph" />
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
  margin-top: 10px;
  padding: 4px;

  > * {
    display: flex;
    padding-left: 8px;
    padding-right: 8px;
  }
}

.OffButton {
  display: flex;
  justify-content: flex-start;
  align-items: center;
  min-width: max-content;
  padding: 0 4px;
  border-radius: var(--radius-full);
  border: none;
  transition: background-color 0.3s;
  &:hover,
  &:focus,
  &:active {
    background-color: var(--color-menu-entry-hover-bg);
  }
  &.disabled {
    cursor: default;
    &:hover {
      background-color: unset;
    }
  }
}

.strikethrough {
  position: relative;
  margin-right: 4px;
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

.partial {
  stroke: grey;
  fill: #808080;
}

.full-format {
  stroke: black;
  fill: #000000;
  justify-content: flex-start;
}
</style>
