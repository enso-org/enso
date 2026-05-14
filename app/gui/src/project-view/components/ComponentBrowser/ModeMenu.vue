<script setup lang="ts">
/**
 * @file Mode-switch button + popover for the Component Browser. Renders an icon for the
 * currently selected mode; clicking the icon opens a dropdown letting the user pick another
 * mode (component search, code edit, or AI). When the mode is locked (i.e. the CB was opened
 * on an existing node and the mode is determined by the node type), only the icon renders —
 * the dropdown does not open. Owns its own round "port" background so the surrounding editor
 * doesn't need to wrap us in another container.
 */
import type { ComponentBrowserMode } from '@/components/ComponentBrowser/input'
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed } from 'vue'

const { selectedMode, aiAvailable, modeLocked, codeEditIcon, asPort } = defineProps<{
  selectedMode: ComponentBrowserMode
  aiAvailable: boolean
  modeLocked: boolean
  /** The icon to display for the "code editing" mode (varies by suggestion / node type). */
  codeEditIcon: Icon
  /**
   * When `true`, render the surrounding node-port background (the round colored disc that
   * stands in for the output port the CB will emit from).
   */
  asPort: boolean
}>()
const emit = defineEmits<{ 'update:selectedMode': [mode: ComponentBrowserMode] }>()

interface ModeOption {
  readonly mode: ComponentBrowserMode
  readonly icon: Icon
  readonly label: string
  readonly disabled: boolean
  readonly title: string
}

const options = computed<readonly ModeOption[]>(() => [
  {
    mode: 'aiPrompt',
    icon: 'robot',
    label: 'AI prompt',
    disabled: !aiAvailable,
    title:
      aiAvailable ?
        'Generate a User Defined Component from a natural-language prompt'
      : 'Claude CLI not found on PATH. Install Claude Code to enable AI mode.',
  },
  {
    mode: 'componentBrowsing',
    icon: 'find',
    label: 'Component search',
    disabled: false,
    title: 'Search the suggestion list',
  },
  {
    mode: 'codeEditing',
    icon: codeEditIcon,
    label: 'Code editing',
    disabled: false,
    title: 'Edit the node as Enso code',
  },
])

const currentIcon = computed<Icon>(() => {
  if (selectedMode === 'aiPrompt') return 'robot'
  if (selectedMode === 'componentBrowsing') return 'find'
  return codeEditIcon
})

function pickMode(mode: ComponentBrowserMode, disabled: boolean): void {
  if (disabled) return
  emit('update:selectedMode', mode)
}
</script>

<template>
  <div v-if="modeLocked" class="ModeMenu locked" :class="{ port: asPort }" @pointerdown.prevent>
    <SvgIcon :name="currentIcon" />
  </div>
  <DropdownMenu
    v-else
    class="ModeMenu"
    :class="{ port: asPort }"
    title="Switch component browser mode"
    showArrow="always"
  >
    <template #button>
      <SvgIcon :name="currentIcon" />
    </template>
    <template #menu>
      <MenuPanel class="modeMenuPanel">
        <MenuButton
          v-for="option in options"
          :key="option.mode"
          class="modeOption"
          :class="{ selected: option.mode === selectedMode }"
          :disabled="option.disabled"
          :title="option.title"
          @activate="pickMode(option.mode, option.disabled)"
        >
          <SvgIcon :name="option.icon" class="optionIcon" />
          <span class="optionLabel">{{ option.label }}</span>
          <SvgIcon v-if="option.mode === selectedMode" name="check" class="checkIcon" />
        </MenuButton>
      </MenuPanel>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.ModeMenu {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  /*
   * Push the arrow indicator further below the port disc so it lands fully on the
   * surrounding white background, instead of straddling the edge of the colored circle.
   */
  --arrow-offset: 4px;
}

/*
 * Paint the port disc on the trigger `MenuButton` itself rather than on this wrapper. The
 * disc then shares its bounds with the button's hover highlight by construction — they
 * occupy the same circle. The icon stays white against the colored disc; the arrow lives
 * outside the button and inherits the app default color so it remains legible.
 */
.ModeMenu.port :deep(.MenuButton) {
  background-color: var(--color-edge-from-node);
  color: white;
}

.ModeMenu.locked {
  cursor: default;
}

.ModeMenu.locked.port {
  background-color: var(--color-edge-from-node);
  color: white;
  border-radius: var(--radius-full);
  padding: var(--button-padding, 4px);
}

.modeMenuPanel {
  min-width: 180px;
  gap: 2px;
  padding: 4px;
}

.modeOption {
  --button-padding: 6px 10px;
  justify-content: flex-start;
  gap: 8px;
}

.modeOption.selected {
  background-color: var(--color-menu-entry-selected-bg, rgb(0 0 0 / 0.05));
}

.optionLabel {
  flex: 1;
  text-align: left;
}

.checkIcon {
  opacity: 0.8;
}
</style>
