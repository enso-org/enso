<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
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
      <MenuButton
        class="full-format"
        :title="`Text displayed in monospace font and all whitespace characters displayed as symbols`"
        @click="() => emit('changeFormat', TextFormatOptions.On)"
      >
        <SvgIcon name="paragraph" />
        <div class="title">Full whitespace rendering</div>
      </MenuButton>

      <MenuButton
        class="partial"
        :title="`Text displayed in monospace font, only multiple spaces displayed with &#183;`"
        @click="() => emit('changeFormat', TextFormatOptions.Partial)"
      >
        <SvgIcon name="paragraph" />
        <div class="title">Partial whitespace rendering</div>
      </MenuButton>

      <MenuButton
        class="off"
        @click="() => emit('changeFormat', TextFormatOptions.Off)"
        title="`No formatting applied to text`"
      >
        <div class="strikethrough">
          <SvgIcon name="paragraph" />
        </div>
        <div class="title">No whitespace rendering</div>
      </MenuButton>
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

.off {
  justify-content: flex-start;
}

.full-format {
  stroke: black;
  fill: #000000;
  justify-content: flex-start;
}

.title {
  padding-left: 2px;
}
</style>
