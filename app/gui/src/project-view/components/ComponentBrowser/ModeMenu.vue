<script setup lang="ts">
/**
 * @file Mode-switch button + popover for the Component Browser. Renders an icon for the
 * currently selected mode; clicking the icon opens a dropdown letting the user pick another
 * mode (component search, code edit, or AI). When the mode is locked (i.e. the CB was opened
 * on an existing node and the mode is determined by the node type), only the icon renders —
 * the dropdown does not open.
 */
import type { SelectedMode } from '@/components/ComponentBrowser/input'
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed } from 'vue'

const { selectedMode, aiAvailable, modeLocked, codeEditIcon } = defineProps<{
  selectedMode: SelectedMode
  aiAvailable: boolean
  modeLocked: boolean
  /** The icon to display for the "code editing" mode (varies by suggestion / node type). */
  codeEditIcon: Icon
}>()
const emit = defineEmits<{ 'update:selectedMode': [mode: SelectedMode] }>()

interface ModeOption {
  readonly mode: SelectedMode
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

function pickMode(mode: SelectedMode): void {
  emit('update:selectedMode', mode)
}
</script>

<template>
  <div v-if="modeLocked" class="ModeMenu locked">
    <SvgIcon :name="currentIcon" />
  </div>
  <DropdownMenu v-else class="ModeMenu" :showArrow="false">
    <template #button>
      <SvgIcon :name="currentIcon" />
    </template>
    <template #menu>
      <div class="modeMenuPanel">
        <MenuButton
          v-for="option in options"
          :key="option.mode"
          class="modeOption"
          :class="{ selected: option.mode === selectedMode }"
          :disabled="option.disabled"
          :title="option.title"
          @activate="!option.disabled && pickMode(option.mode)"
        >
          <SvgIcon :name="option.icon" class="optionIcon" />
          <span class="optionLabel">{{ option.label }}</span>
          <SvgIcon v-if="option.mode === selectedMode" name="check" class="checkIcon" />
        </MenuButton>
      </div>
    </template>
  </DropdownMenu>
</template>

<style scoped>
.ModeMenu {
  display: inline-flex;
  align-items: center;
}

.ModeMenu.locked {
  cursor: default;
}

.modeMenuPanel {
  background-color: var(--color-app-bg, #fff);
  border-radius: var(--radius-default, 8px);
  padding: 4px;
  display: flex;
  flex-direction: column;
  gap: 2px;
  box-shadow: 0 4px 16px rgb(0 0 0 / 0.15);
  min-width: 180px;
}

.modeOption {
  --button-right-radius: var(--radius-default, 6px);
  --button-left-radius: var(--radius-default, 6px);
  justify-content: flex-start;
  gap: 8px;
  padding: 6px 10px;
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
