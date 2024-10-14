<script setup lang="ts">
import TooltipTrigger from '@/components/TooltipTrigger.vue'

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
const props = defineProps<{ disabled?: boolean | undefined; title?: string | undefined }>()

function onClick() {
  if (!props.disabled && toggledOn.value != null) toggledOn.value = !toggledOn.value
}
</script>

<template>
  <TooltipTrigger>
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
  padding: 4px;
  border-radius: var(--radius-full);
  border: none;
  transition: background-color 0.3s;
  &:hover,
  &:focus,
  &:active {
    background-color: var(--color-menu-entry-hover-bg);
  }
  &.disabled {
    cursor: default;
    &:hover {
      background-color: unset;
    }
  }
}
</style>
