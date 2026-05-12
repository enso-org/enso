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
import SvgIcon from '@/components/SvgIcon.vue'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { usePopoverRoot } from '@/providers/popoverRoot'
import { endOnClickOutside } from '@/util/autoBlur'
import type { Icon } from '@/util/iconMetadata/iconName'
import { shift, useFloating } from '@floating-ui/vue'
import { computed, shallowRef } from 'vue'

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

const trigger = shallowRef<HTMLElement>()
const popover = shallowRef<HTMLElement>()
const popoverRoot = usePopoverRoot(true)
const open = shallowRef(false)
const { floatingStyles } = useFloating(trigger, popover, {
  placement: () => 'bottom-start',
  middleware: [shift()],
})

const closePopover = () => {
  open.value = false
}
const interaction = endOnClickOutside(popover, {
  cancel: closePopover,
  end: closePopover,
  parentInteraction: undefined,
})
injectInteractionHandler().setWhenWithParent(open, (parentInteraction) => {
  interaction.parentInteraction = parentInteraction
  return interaction
})

function toggleOpen() {
  if (modeLocked) return
  open.value = !open.value
}

function pickMode(mode: ComponentBrowserMode, disabled: boolean) {
  if (disabled) return
  open.value = false
  emit('update:selectedMode', mode)
}
</script>

<template>
  <div
    ref="trigger"
    class="ModeMenu"
    :class="{ port: asPort, locked: modeLocked, interactive: !modeLocked }"
    :title="modeLocked ? undefined : 'Switch component browser mode'"
    @pointerdown.prevent
    @click="toggleOpen"
  >
    <SvgIcon :name="currentIcon" class="modeIcon" />
    <SvgIcon v-if="!modeLocked" name="arrow_right_head_only" class="arrow" />
    <Teleport :to="popoverRoot ?? 'body'">
      <div
        v-if="open"
        ref="popover"
        class="ModeMenuPopover"
        :style="floatingStyles"
        @pointerdown.prevent
      >
        <button
          v-for="option in options"
          :key="option.mode"
          type="button"
          class="modeOption"
          :class="{ selected: option.mode === selectedMode, disabled: option.disabled }"
          :disabled="option.disabled"
          :title="option.title"
          @click.stop="pickMode(option.mode, option.disabled)"
        >
          <SvgIcon :name="option.icon" class="optionIcon" />
          <span class="optionLabel">{{ option.label }}</span>
          <SvgIcon v-if="option.mode === selectedMode" name="check" class="checkIcon" />
        </button>
      </div>
    </Teleport>
  </div>
</template>

<style scoped>
.ModeMenu {
  position: relative;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: var(--icon-size, 16px);
  height: var(--icon-size, 16px);
  padding: var(--port-padding, 4px);
  border-radius: var(--radius-full);
  box-sizing: content-box;
  isolation: isolate;
}

.ModeMenu.port {
  background-color: var(--color-edge-from-node);
  color: white;
}

.ModeMenu.interactive {
  cursor: pointer;
}

.modeIcon {
  display: block;
}

.arrow {
  position: absolute;
  bottom: -8px;
  left: 50%;
  opacity: 0.8;
  pointer-events: none;
  --icon-transform: translateX(-50%) rotate(90deg) scale(0.7);
  --icon-transform-origin: center;
}

.ModeMenuPopover {
  background-color: var(--color-app-bg, #fff);
  border-radius: var(--radius-default, 8px);
  padding: 4px;
  display: flex;
  flex-direction: column;
  gap: 2px;
  box-shadow: 0 4px 16px rgb(0 0 0 / 0.15);
  min-width: 180px;
  z-index: var(--drop-down-panel-z-index, 20);
}

.modeOption {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 6px 10px;
  border: none;
  background: transparent;
  border-radius: var(--radius-default, 6px);
  text-align: left;
  font: inherit;
  color: inherit;
  cursor: pointer;
}

.modeOption:hover:not(.disabled) {
  background-color: var(--color-menu-entry-hover-bg, rgb(0 0 0 / 0.05));
}

.modeOption.selected {
  background-color: var(--color-menu-entry-selected-bg, rgb(0 0 0 / 0.05));
}

.modeOption.disabled {
  cursor: default;
  opacity: 0.4;
}

.optionLabel {
  flex: 1;
  text-align: left;
}

.checkIcon {
  opacity: 0.8;
}
</style>
