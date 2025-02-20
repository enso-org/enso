<script setup lang="ts">
import ColorPickerMenu from '@/components/ColorPickerMenu.vue'
import SelectionButton from '@/components/SelectionButton.vue'
import { injectSelectionButtons } from '@/providers/selectionButtons'

const { selectedNodeCount, buttons } = injectSelectionButtons()
const { pickColorMulti } = buttons
</script>

<template>
  <Transition>
    <div v-if="selectedNodeCount > 1" class="SelectionMenu">
      <span v-text="`${selectedNodeCount} components selected`" />
      <SelectionButton button="collapse" />
      <SelectionButton
        button="pickColorMulti"
        :class="{
          // Any `pointerdown` event outside the color picker will close it. Ignore clicks that occur while the color
          // picker is open, so that it isn't toggled back open.
          disableInput: pickColorMulti.state,
        }"
      />
      <SelectionButton button="copy" />
      <SelectionButton button="deleteSelected" />
      <ColorPickerMenu
        v-if="pickColorMulti.state"
        class="submenu"
        @close="pickColorMulti.state = false"
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
