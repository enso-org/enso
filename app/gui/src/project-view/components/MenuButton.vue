<script setup lang="ts">
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { ref } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'

/**
 * A button. Supports toggling and disabled state.
 *
 * If a boolean model is bound to the primary model, clicking the button will switch between `toggledOn` and
 * `toggledOff` css classes on the slot's root element, as well as updating the model.
 *
 * If the disabled property is set, the button stops responding to mouse interaction and its contents will have the
 * `disabled` class.
 */

const toggledOn = defineModel<boolean>({ default: undefined })
const props = defineProps<{
  disabled?: boolean | undefined
  title?: string | undefined
  extendedHover?: number | undefined
}>()
const tooltipTrigger = ref<ComponentExposed<typeof TooltipTrigger>>()

function onClick() {
  if (!props.disabled && toggledOn.value != null) toggledOn.value = !toggledOn.value
  if (tooltipTrigger.value) {
    tooltipTrigger.value.hideTooltip()
  }
}
</script>

<template>
  <TooltipTrigger ref="tooltipTrigger">
    <template #default="triggerProps">
      <button
        class="MenuButton clickable"
        :aria-label="props.title ?? ''"
        :class="{ toggledOn, toggledOff: toggledOn === false, disabled }"
        :disabled="disabled ?? false"
        v-bind="triggerProps"
        @click.stop="onClick"
      >
        <slot />
        <div
          v-if="extendedHover"
          class="hoverArea"
          :style="{ '--extendedHover': `${extendedHover}px` }"
        />
      </button>
    </template>
    <template v-if="$slots.tooltip || props.title" #tooltip>
      <slot name="tooltip">{{ props.title }}</slot>
    </template>
  </TooltipTrigger>
</template>

<style scoped>
.MenuButton {
  display: flex;
  justify-content: center;
  align-items: center;
  min-width: max-content;
  padding: var(--button-padding, 4px);
  border-radius: var(--radius-full);
  border: none;
  transition: background-color 0.3s;
  position: relative;

  &.toggledOn {
    background-color: var(--color-menu-entry-selected-bg);
  }

  &:hover,
  &:focus,
  &:active {
    background-color: var(--color-menu-entry-hover-bg);
  }
  &.disabled {
    cursor: default;
    opacity: 0.4;
    &:hover {
      background-color: unset;
    }
  }
}

.hoverArea {
  position: absolute;
  inset: calc(var(--extendedHover) * -1);
  cursor: pointer;
}
</style>
