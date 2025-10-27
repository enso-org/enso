<script setup lang="ts">
import type { FileType } from '$/providers/openedProjects/widgetRegistry/configuration'
import SelectionSubmenu, {
  type SubmenuComponent,
} from '@/components/GraphEditor/widgets/WidgetSelection/SelectionSubmenu.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import type { Filter } from '@/components/widgets/FileBrowserWidget/fileExtensionFilter'
import { useFileExtensions } from '@/components/widgets/FileBrowserWidget/fileExtensions'
import type { Opt } from '@/util/data/opt'
import { toRef, useTemplateRef } from 'vue'

const filenameInput = defineModel<string>('filenameInput', { required: true })
const extensionInput = defineModel<string>('extensionInput', {
  required: true,
})

const props = defineProps<{
  writeMode: boolean
  fileExtensionFilter: Filter
  displayedExtension: string
  fileTypes: FileType[]
  root: Opt<HTMLElement>
}>()
const emit = defineEmits<{
  accept: []
  setFilter: [Filter]
}>()

const { extensionMenu, extensionInputField } = useFileExtensions({
  filenameInput,
  extensionInput,
  fileExtensionInput: useTemplateRef<InstanceType<typeof AutoSizedInput>>('fileExtensionInput'),
  fileExtensionInputRoot: useTemplateRef<HTMLDivElement>('fileExtensionInputRoot'),
  submenuRef: useTemplateRef<SubmenuComponent>('submenuRef'),
  fileExtensionFilter: toRef(props, 'fileExtensionFilter'),
  displayedExtension: toRef(props, 'displayedExtension'),
  fileTypes: toRef(props, 'fileTypes'),
  root: toRef(props, 'root'),
  emit,
})
</script>

<template>
  <div class="FileBrowserNameBar">
    <input
      v-if="writeMode"
      v-model="filenameInput"
      class="inputField"
      @pointerdown.stop
      @click.stop
      @contextmenu.stop
      @keydown.backspace.stop
      @keydown.delete.stop
      @keydown.arrow-left.stop
      @keydown.arrow-right.stop
      @keydown.enter.stop="emit('accept')"
    />
    <div v-else class="expander"></div>
    <div
      v-if="writeMode && fileExtensionFilter.type !== 'predefined'"
      class="fileExtensionSeparator"
    ></div>
    <div ref="fileExtensionInputRoot" class="fileExtensionInputContainer">
      <SvgIcon
        name="arrow_right_head_only"
        class="arrow widgetOutOfLayout"
        :class="{ hovered: false }"
      />
      <AutoSizedInput ref="fileExtensionInput" v-bind="extensionInputField" class="inputField" />
    </div>
    <SvgButton
      v-if="writeMode"
      class="FileBrowserButton"
      label="Ok"
      :disabled="!filenameInput"
      @click.stop="emit('accept')"
    />
  </div>
  <SelectionSubmenu ref="submenuRef" v-bind="extensionMenu" />
</template>

<style scoped>
.FileBrowserNameBar {
  width: 100%;
  display: flex;
  flex-direction: row;
  padding: var(--border-width) 0 0 0;
  gap: var(--border-width);
}

.expander {
  flex-grow: 1;
}

.inputField {
  border-radius: var(--border-radius-inner);
  height: calc(var(--border-radius-inner) * 2);
  padding: 0 8px;
  background-color: var(--color-frame-selected-bg);
  flex-grow: 1;
  appearance: textfield;
  -moz-appearance: textfield;
  user-select: all;
}

.fileExtensionSeparator {
  width: 0;
  position: relative;
  &::before {
    content: '.';
    font-size: 26px;
    color: var(--color-frame-selected-bg);
    position: absolute;
    left: -4px;
    bottom: -6px;
  }
}

.fileExtensionInputContainer {
  position: relative;
}

svg.arrow {
  position: absolute;
  bottom: -8px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  transform-origin: center;
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
  &.hovered {
    opacity: 0.9;
  }
}
</style>
