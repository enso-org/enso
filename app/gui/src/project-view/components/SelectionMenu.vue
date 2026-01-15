<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ColorPickerMenu from '@/components/ColorPickerMenu.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { resolveAction } from '@/providers/action'
import { useGraphSelection } from '@/providers/graphSelection'
import { flip, offset, shift, useFloating } from '@floating-ui/vue'
import { computed, nextTick, ref, toValue, watch } from 'vue'

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
const alignmentMenuOpen = ref(false)
const alignmentMenuOpenedByHover = ref(false)
const alignmentMenuHovering = ref(false)
const alignmentMenuOpenModel = computed({
  get: () => alignmentMenuOpen.value,
  set: (open) => {
    if (!open && alignmentMenuOpenedByHover.value && alignmentMenuHovering.value) return
    alignmentMenuOpen.value = open
    if (!open) alignmentMenuOpenedByHover.value = false
  },
})
let alignmentMenuOpenTimeout: number | undefined
let alignmentMenuCloseTimeout: number | undefined

function clearAlignmentMenuOpenTimeout() {
  if (alignmentMenuOpenTimeout != null) {
    window.clearTimeout(alignmentMenuOpenTimeout)
    alignmentMenuOpenTimeout = undefined
  }
}

function clearAlignmentMenuCloseTimeout() {
  if (alignmentMenuCloseTimeout != null) {
    window.clearTimeout(alignmentMenuCloseTimeout)
    alignmentMenuCloseTimeout = undefined
  }
}

function scheduleAlignmentMenuOpen() {
  clearAlignmentMenuCloseTimeout()
  clearAlignmentMenuOpenTimeout()
  alignmentMenuOpenTimeout = window.setTimeout(() => {
    if (!alignmentMenuHovering.value) return
    alignmentMenuOpenedByHover.value = true
    alignmentMenuOpen.value = true
  }, 200)
}

function scheduleAlignmentMenuClose() {
  clearAlignmentMenuOpenTimeout()
  clearAlignmentMenuCloseTimeout()
  if (!alignmentMenuOpenedByHover.value) return
  alignmentMenuCloseTimeout = window.setTimeout(() => {
    if (alignmentMenuHovering.value) return
    alignmentMenuOpenedByHover.value = false
    alignmentMenuOpen.value = false
  }, 150)
}

function handleAlignmentMenuEnter() {
  alignmentMenuHovering.value = true
  scheduleAlignmentMenuOpen()
}

function handleAlignmentMenuLeave() {
  alignmentMenuHovering.value = false
  scheduleAlignmentMenuClose()
}

watch(alignmentMenuOpen, (open) => {
  if (!open) alignmentMenuOpenedByHover.value = false
})
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
    <DropdownMenu
      v-model:open="alignmentMenuOpenModel"
      placement="bottom-start"
      title="Align"
      alwaysShowArrow
      @mouseenter="handleAlignmentMenuEnter"
      @mouseleave="handleAlignmentMenuLeave"
      @pointerenter="handleAlignmentMenuEnter"
      @pointerleave="handleAlignmentMenuLeave"
    >
      <template #button>
        <SvgIcon name="align_left" />
      </template>
      <template #menu>
        <div
          class="alignmentMenuHover"
          @mouseenter="handleAlignmentMenuEnter"
          @mouseleave="handleAlignmentMenuLeave"
          @pointerenter="handleAlignmentMenuEnter"
          @pointerleave="handleAlignmentMenuLeave"
        >
          <MenuPanel class="alignmentMenu">
            <div class="alignmentMenuRow horizontal">
              <ActionButton action="components.alignLeft" @click="alignmentMenuOpen = false" />
              <ActionButton action="components.alignCenter" @click="alignmentMenuOpen = false" />
              <ActionButton action="components.alignRight" @click="alignmentMenuOpen = false" />
            </div>
            <div class="alignmentMenuRow vertical">
              <ActionButton action="components.alignTop" @click="alignmentMenuOpen = false" />
              <ActionButton action="components.alignBottom" @click="alignmentMenuOpen = false" />
            </div>
          </MenuPanel>
        </div>
      </template>
    </DropdownMenu>
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

.alignmentMenu {
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 10px 12px;
}

.alignmentMenuRow {
  display: flex;
  gap: 10px;
}


.disableInput {
  pointer-events: none;
}
</style>
