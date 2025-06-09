<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ColorPickerMenu from '@/components/ColorPickerMenu.vue'
import { resolveAction } from '@/providers/action'
import { injectGraphSelection } from '@/providers/graphSelection'
import { toValue } from 'vue'

const selection = injectGraphSelection()
const pickColorMulti = resolveAction('components.pickColorMulti')
</script>

<template>
  <div class="SelectionMenu">
    <span v-text="`${selection.selected.size} components selected`" />
    <ActionButton action="components.collapse" />
    <ActionButton
      action="components.pickColorMulti"
      :class="{
        // Any `pointerdown` event outside the color picker will close it. Ignore clicks that occur while the color
        // picker is open, so that it isn't toggled back open.
        disableInput: toValue(pickColorMulti.toggled),
      }"
    />
    <ActionButton action="components.copy" />
    <ActionButton action="components.deleteSelected" />
    <ColorPickerMenu
      v-if="toValue(pickColorMulti.toggled)"
      class="submenu"
      @close="pickColorMulti.action?.()"
    />
  </div>
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
</style>
