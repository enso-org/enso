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
        class="on"
        lable="Full whitespace rendering"
        :title="`Text displayed in monaspace font and all whitespace characters displayed as symbols`"
        @click.stop="() => emit('changeFormat', TextFormatOptions.On)"
      />
      <SvgButton
        name="paragraph"
        class="partial"
        lable="Partial whitespace rendering"
        :title="`Text displayed in monaspace font, only multiple spaces displayed with &#183;`"
        @click.stop="() => emit('changeFormat', TextFormatOptions.Partial)"
      />
      <SvgButton
        name="paragraph"
        class="strikethrough"
        lable="No whitespace rendering"
        :title="`No formatting applied to text`"
        @click.stop="() => emit('changeFormat', TextFormatOptions.Off)"
      />
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

.partial {
  stroke: grey;
  fill: #808080;
}

.on {
  stroke: black;
  fill: #000000;
}
</style>
