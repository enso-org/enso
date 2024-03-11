<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { isMacLike } from '@/composables/events'
import { ref } from 'vue'

const isDropdownOpen = ref(false)

const props = defineProps<{
  zoomLevel: number
}>()
const emit = defineEmits<{ zoomIn: []; zoomOut: []; fitToAllClicked: []; toggleCodeEditor: [] }>()

const toggleCodeEditorShortcut = isMacLike ? 'Cmd + `' : 'Ctrl + `'
</script>

<template>
  <div class="ExtendedMenu">
    <div class="moreIcon" @pointerdown="isDropdownOpen = !isDropdownOpen">…</div>
    <Transition name="dropdown">
      <div v-show="isDropdownOpen" class="ExtendedMenuPane">
        <div class="row">
          <div class="label">Zoom</div>
          <div class="zoomControl">
            <div
              class="zoomButton minus"
              title="Decrease zoom"
              @pointerdown.stop="emit('zoomOut')"
            />
            <span
              class="zoomScaleLabel"
              v-text="props.zoomLevel ? props.zoomLevel.toFixed(0) + '%' : '?'"
            ></span>
            <div class="zoomButton plus" title="increase zoom" @pointerdown.stop="emit('zoomIn')" />
            <div class="divider"></div>
            <div class="showAllIconHighlight">
              <SvgIcon
                name="show_all"
                class="showAllIcon"
                @pointerdown.stop="emit('fitToAllClicked')"
              />
            </div>
          </div>
        </div>
        <div class="row clickableRow" @pointerdown.stop="emit('toggleCodeEditor')">
          <div class="label">Code Editor</div>
          <div>{{ toggleCodeEditorShortcut }}</div>
        </div>
      </div>
    </Transition>
  </div>
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
}

.ExtendedMenuPane {
  position: fixed;
  display: flex;
  flex-direction: column;
  width: 250px;
  top: 32px;
  margin-top: 6px;
  padding: 4px;
  right: 0px;
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

.showAllIconHighlight {
  display: flex;
  justify-items: center;
  align-items: center;
  padding-left: 4px;
  cursor: pointer;
  width: 24px;
  height: 24px;
  margin: -4px -4px;
  border-radius: var(--radius-full);
  transition: background-color 0.3s;
  &:hover {
    background-color: var(--color-menu-entry-hover-bg);
  }
}

.zoomScaleLabel {
  width: 4em;
  text-align: center;
}

.moreIcon {
  width: 32px;
  height: 32px;
  text-align: center;
  font-size: 24px;
  font-family: var(--font-code);
  transform: rotate(90deg);
  position: relative;
  left: 2px;
}

.zoomButton {
  width: 16px;
  height: 16px;
  border-radius: var(--radius-full);
  position: relative;
  margin: 0px;
  display: inline-block;
  vertical-align: middle;
  cursor: pointer;
  transition: background-color 0.3s;
}

.zoomButton:hover {
  background-color: var(--color-menu-entry-hover-bg);
}

.zoomButton.plus:before,
.zoomButton.plus:after {
  content: '';
  position: absolute;
  left: 50%;
  top: 50%;
  transform: translate(-50%, -50%);
  background: var(--color-text);
}

.zoomButton.plus:before {
  width: 2px;
  height: 12px;
}

.zoomButton.plus:after {
  height: 2px;
  width: 12px;
}

.zoomButton.minus:before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: var(--color-text);
  margin: auto 2px;
  height: 2px;
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
