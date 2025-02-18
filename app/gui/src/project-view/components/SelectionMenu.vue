<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ColorPickerMenu from '@/components/ColorPickerMenu.vue'
import { injectActions } from '@/providers/action'
import { injectGraphSelection } from '@/providers/graphSelection'

const actions = injectActions()
const { selected } = injectGraphSelection()
const pickColorMulti = actions['components.pickColorMulti']
</script>

<template>
  <Transition>
    <div v-if="selected.size > 1" class="SelectionMenu">
      <span v-text="`${selected.size} components selected`" />
      <ActionButton action="components.collapse" />
      <ActionButton
        action="components.pickColorMulti"
        :class="{
          // Any `pointerdown` event outside the color picker will close it. Ignore clicks that occur while the color
          // picker is open, so that it isn't toggled back open.
          disableInput: pickColorMulti.toggled,
        }"
      />
      <ActionButton action="components.copy" />
      <ActionButton action="components.deleteSelected" />
      <ColorPickerMenu
        v-if="pickColorMulti.toggled"
        class="submenu"
        @close="pickColorMulti.toggled.value = false"
      />
    </div>
  </Transition>
</template>

<style scoped>
.SelectionMenu {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding: 4px 10px;
}

.submenu {
  position: absolute;
  top: 36px;
  left: 0;
  border-radius: var(--radius-default);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
}

.toggledOff svg {
  opacity: 0.6;
}

.disableInput {
  pointer-events: none;
}

.v-enter-active,
.v-leave-active {
  transition: opacity 0.25s ease;
}

.v-enter-from,
.v-leave-to {
  opacity: 0;
}
</style>
