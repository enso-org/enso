<script setup lang="ts">
import SelectionSubmenu, {
  SubmenuComponent,
} from '@/components/GraphEditor/widgets/WidgetSelection/SelectionSubmenu.vue'
import { SubmenuEntry } from '@/components/GraphEditor/widgets/WidgetSelection/submenuEntry'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { injectInteractionHandler, Interaction } from '@/providers/interactionHandler'
import {
  FileType,
  isExtensions,
  isFileTypes,
  isGlobAll,
} from '@/providers/widgetRegistry/configuration'
import { endOnClick, targetIsOutside } from '@/util/autoBlur'
import { Opt } from '@/util/data/opt'
import { computed, ref, useTemplateRef } from 'vue'
import { Filter } from './fileExtensionFilter'

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

// === File Extension Filter ===

const interaction = injectInteractionHandler()
const fileExtensionDropdownOpened = ref(false)
const fileExtensionInputRoot = useTemplateRef<HTMLDivElement>('fileExtensionInputRoot')
const submenuRef = useTemplateRef<SubmenuComponent>('submenuRef')
const fileExtensionInput = useTemplateRef<InstanceType<typeof AutoSizedInput>>('fileExtensionInput')

const fileExtensionEntries = computed(() => props.fileTypes.map(fileTypeToFileExtensionEntry))

function isSelected(value: string): boolean {
  if (props.fileExtensionFilter.type === 'glob') return false
  if (props.fileExtensionFilter.type === 'userInput')
    return props.fileExtensionFilter.input === value
  return props.fileExtensionFilter.label === value
}

function fileTypeToFileExtensionEntry(fileType: FileType): FileExtensionEntry {
  const nestedValues =
    isFileTypes(fileType.extensions) ? fileType.extensions.map(fileTypeToFileExtensionEntry) : []
  const extensions =
    isGlobAll(fileType.extensions) ? 'all'
    : isExtensions(fileType.extensions) ? fileType.extensions
    : []
  return {
    value: fileType.label,
    extensions,
    selected: isSelected(fileType.label),
    isNested: nestedValues.length > 0,
    nestedValues: nestedValues,
  }
}

function isOutsideDropdown(event: Event) {
  return submenuRef.value?.isTargetOutside(event) ?? false
}

function isOutsideWidget(event: Event) {
  return targetIsOutside(event, props.root)
}

// Close the dropdown when clicking outside of it, but also end parent interaction (file browser widget) when clicking outside of both.
const fileExtensionDropdownInteraction: Interaction = endOnClick(
  (event) => isOutsideDropdown(event) && !isOutsideWidget(event),
  {
    cancel: () => {
      fileExtensionDropdownOpened.value = false
    },
    end: () => {
      fileExtensionDropdownOpened.value = false
    },
    pointerdown: (event) => {
      if (
        isOutsideDropdown(event) &&
        isOutsideWidget(event) &&
        fileExtensionDropdownInteraction.parentInteraction
      ) {
        interaction.end(fileExtensionDropdownInteraction.parentInteraction)
      }
    },
  },
)

interaction.setWhenWithParent(
  () => fileExtensionDropdownOpened.value,
  (parentInteraction) => {
    fileExtensionDropdownInteraction.parentInteraction = parentInteraction
    return fileExtensionDropdownInteraction
  },
)

function openDropdown() {
  if (!fileExtensionDropdownOpened.value) {
    fileExtensionDropdownOpened.value = true
  }
  fileExtensionInput.value?.select()
}

function extensionSelected(entry: FileExtensionEntry) {
  interaction.end(fileExtensionDropdownInteraction)
  if (filenameInput.value !== entry.value) {
    filenameInput.value = ''
  }
  if (entry.extensions === 'all' || entry.extensions.length === 0) {
    emit('setFilter', {
      type: 'glob',
    })
  } else {
    emit('setFilter', {
      type: 'predefined',
      label: entry.value,
      extensions: entry.extensions,
    })
  }
}

const fileExtensionInputModel = computed({
  get: () => {
    if (props.fileExtensionFilter.type === 'userInput') {
      return props.fileExtensionFilter.input
    }
    return props.displayedExtension
  },
  set: (value) => {
    extensionInput.value = value
  },
})

interface FileExtensionEntry extends SubmenuEntry<FileExtensionEntry> {
  extensions: 'all' | string[]
}
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
      <AutoSizedInput
        ref="fileExtensionInput"
        v-model="fileExtensionInputModel"
        class="inputField"
        @click="openDropdown()"
      />
    </div>
    <SvgButton
      v-if="writeMode"
      class="FileBrowserButton"
      label="Ok"
      :disabled="!filenameInput"
      @click.stop="emit('accept')"
    />
  </div>
  <SelectionSubmenu
    ref="submenuRef"
    :rootElement="root"
    :floatReference="fileExtensionInputRoot"
    :show="fileExtensionDropdownOpened"
    :entries="fileExtensionEntries"
    :isSelected="() => false"
    :topLevel="true"
    :color="'white'"
    :backgroundColor="'var(--background-color)'"
    :style="{ zIndex: -5 }"
    @clickedEntry="extensionSelected"
  />
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
  &::before {
    content: '.';
    font-size: 26px;
    color: var(--color-frame-selected-bg);
    position: relative;
    left: -4px;
    bottom: -4px;
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
