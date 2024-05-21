<script setup lang="ts">
import { codeEditorBindings, documentationEditorBindings } from '@/bindings'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { targetIsOutside } from '@/util/autoBlur'
import { ref } from 'vue'
import SvgButton from './SvgButton.vue'

const showCodeEditor = defineModel<boolean>('showCodeEditor', { required: true })
const showDocumentationEditor = defineModel<boolean>('showDocumentationEditor', { required: true })
const props = defineProps<{
  zoomLevel: number
}>()
const emit = defineEmits<{
  zoomIn: []
  zoomOut: []
  fitToAllClicked: []
}>()

const buttonElement = ref<HTMLElement>()
const menuElement = ref<HTMLElement>()

const isDropdownOpen = ref(false)

const interaction = injectInteractionHandler()
const dropdownInteraction: Interaction = {
  cancel: () => (isDropdownOpen.value = false),
  end: () => (isDropdownOpen.value = false),
  pointerdown: (e: PointerEvent) => {
    if ([buttonElement.value, menuElement.value].every((area) => targetIsOutside(e, area)))
      closeDropdown()
    return false
  },
}
function openDropdown() {
  isDropdownOpen.value = true
  interaction.setCurrent(dropdownInteraction)
}
function closeDropdown() {
  interaction.cancel(dropdownInteraction)
}
function toggleDropdown() {
  isDropdownOpen.value ? closeDropdown() : openDropdown()
}

const toggleCodeEditorShortcut = codeEditorBindings.bindings.toggle.humanReadable
const toggleDocumentationEditorShortcut = documentationEditorBindings.bindings.toggle.humanReadable
</script>

<template>
  <div ref="buttonElement" class="ExtendedMenu" @click.stop="toggleDropdown">
    <SvgIcon name="3_dot_menu" class="moreIcon" />
  </div>
  <Transition name="dropdown">
    <div v-show="isDropdownOpen" ref="menuElement" class="ExtendedMenuPane">
      <div class="row">
        <div class="label">Zoom</div>
        <div class="zoomControl">
          <SvgButton
            class="zoomButton"
            name="minus"
            title="Decrease zoom"
            @click="emit('zoomOut')"
          />
          <span
            class="zoomScaleLabel"
            v-text="props.zoomLevel ? props.zoomLevel.toFixed(0) + '%' : '?'"
          ></span>
          <SvgButton class="zoomButton" name="add" title="Increase zoom" @click="emit('zoomIn')" />
          <div class="divider"></div>
          <SvgButton name="show_all" class="showAllIcon" @click="emit('fitToAllClicked')" />
        </div>
      </div>
      <div
        class="row clickableRow"
        :class="{ selected: showCodeEditor }"
        @click="(showCodeEditor = !showCodeEditor), closeDropdown()"
      >
        <div class="label">Code Editor</div>
        <div v-text="toggleCodeEditorShortcut" />
      </div>
      <div
        class="row clickableRow"
        :class="{ selected: showDocumentationEditor }"
        @click="(showDocumentationEditor = !showDocumentationEditor), closeDropdown()"
      >
        <div class="label">Documentation Editor</div>
        <div v-text="toggleDocumentationEditorShortcut" />
      </div>
    </div>
  </Transition>
</template>

<style scoped>
.ExtendedMenu {
  display: flex;
  place-items: center;
  gap: 12px;
  width: 32px;
  height: 32px;
  margin-left: auto;
  margin-right: 125px;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  cursor: pointer;
}

.ExtendedMenuPane {
  position: fixed;
  display: flex;
  flex-direction: column;
  width: 250px;
  top: 40px;
  margin-top: 6px;
  padding: 4px;
  right: 8px;
  border-radius: 12px;
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
}

.clickableRow {
  cursor: pointer;
  transition: background-color 0.3s;
  &:hover {
    background-color: var(--color-menu-entry-hover-bg);
  }
  &:active {
    background-color: var(--color-menu-entry-active-bg);
  }
  &.selected {
    background-color: var(--color-menu-entry-selected-bg);
  }
}

.label {
  user-select: none;
  pointer-events: none;
}

.row {
  width: 100%;
  display: flex;
  padding: 0 8px 0 8px;
  justify-content: space-between;
  align-items: center;
  border-radius: 12px;
}

.divider {
  border-left: 1px solid var(--color-text);
  border-right: 1px solid var(--color-text);
  height: 17px;
  margin-left: 10px;
  margin-right: 10px;
  opacity: 0.3;
}

.zoomControl {
  display: flex;
  gap: 4px;
  align-items: center;
}

.zoomScaleLabel {
  width: 4em;
  text-align: center;
}

.moreIcon {
  position: relative;
  left: 8px;
}

.zoomButton {
  --icon-transform: scale(12/16);
}

.dropdown-enter-active,
.dropdown-leave-active {
  transition: opacity 0.25s ease;
}

.dropdown-enter-from,
.dropdown-leave-to {
  opacity: 0;
}
</style>
