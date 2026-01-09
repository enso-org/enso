<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ColorPickerMenu from '@/components/ColorPickerMenu.vue'
import { resolveAction } from '@/providers/action'
import { useGraphSelection } from '@/providers/graphSelection'
import { flip, offset, shift, useFloating } from '@floating-ui/vue'
import { nextTick, ref, toValue, watch } from 'vue'

const selection = useGraphSelection()
const pickColorMulti = resolveAction('components.pickColorMulti')

const colorButtonRef = ref<HTMLElement>()
const colorMenuRef = ref<HTMLElement>()
const { floatingStyles, update } = useFloating(colorButtonRef, colorMenuRef, {
  placement: 'bottom-start',
  strategy: 'fixed',
  middleware: [offset(6), flip(), shift({ padding: 8 })],
})

watch(
  () => toValue(pickColorMulti.toggled),
  (opened) => {
    if (opened) nextTick(update)
  },
)
</script>

<template>
  <div class="SelectionMenu">
    <span v-text="`${selection.selected.size} components selected`" />
    <ActionButton action="components.collapse" />
    <span ref="colorButtonRef">
      <ActionButton
        action="components.pickColorMulti"
        :class="{
          // Any `pointerdown` event outside the color picker will close it. Ignore clicks that occur while the color
          // picker is open, so that it isn't toggled back open.
          disableInput: toValue(pickColorMulti.toggled),
        }"
      />
    </span>
    <ActionButton action="components.alignLeft" />
    <ActionButton action="components.alignCenter" />
    <ActionButton action="components.alignRight" />
    <ActionButton action="components.alignTop" />
    <ActionButton action="components.alignBottom" />
    <ActionButton action="components.copy" />
    <ActionButton action="components.deleteSelected" />
    <Teleport to="body">
      <ColorPickerMenu
        v-if="toValue(pickColorMulti.toggled)"
        ref="colorMenuRef"
        class="submenu"
        :style="floatingStyles"
        @close="pickColorMulti.action?.()"
      />
    </Teleport>
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
  border-radius: var(--radius-default);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
}

.disableInput {
  pointer-events: none;
}
</style>
