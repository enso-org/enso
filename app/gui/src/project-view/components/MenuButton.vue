<script setup lang="ts">
import TooltipTrigger from '@/components/TooltipTrigger.vue'
import { useStateBeforePointerdown } from '@/composables/events'
import { computed, ref, useTemplateRef } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'

/**
 * A button. Supports toggling and disabled state.
 *
 * If a boolean model is bound to the primary model, clicking the button will switch between
 * `toggledOn` and `toggledOff` css classes on the slot's root element, as well as updating the
 * model.
 *
 * If the disabled property is set, the button stops responding to mouse interaction and its
 * contents will have the `disabled` class.
 */

const toggledOn = defineModel<boolean | undefined>()
const props = defineProps<{
  disabled?: boolean | undefined
  title?: string | undefined
  extraClickZone?: number | undefined
}>()
const tooltipTrigger = ref<ComponentExposed<typeof TooltipTrigger>>()
const emit = defineEmits<{ activate: [] }>()

const style = computed(() =>
  props.extraClickZone != null ? { '--extraClickZone': `${props.extraClickZone}px` } : {},
)

const button = useTemplateRef<HTMLElement>('button')
const { stateBeforeClick } = useStateBeforePointerdown(button, () => toggledOn.value)

function onActivate() {
  tooltipTrigger.value?.hideTooltip()
  if (props.disabled) return
  if (stateBeforeClick.value != null) toggledOn.value = !stateBeforeClick.value
  emit('activate')
}
</script>

<template>
  <TooltipTrigger ref="tooltipTrigger">
    <template #default="triggerProps">
      <button
        ref="button"
        class="MenuButton clickable"
        :aria-label="props.title ?? ''"
        :class="{ toggledOn, toggledOff: toggledOn === false, disabled }"
        :style="style"
        :disabled="disabled ?? false"
        v-bind="triggerProps"
        type="button"
        @pointerdown.prevent
        @click.stop="onActivate"
        @keydown.enter.stop
      >
        <slot />
        <div v-if="extraClickZone" class="hoverArea" />
      </button>
    </template>
    <template v-if="$slots.tooltip || props.title" #tooltip>
      <slot name="tooltip">{{ props.title }}</slot>
    </template>
  </TooltipTrigger>
</template>

<style scoped>
.MenuButton {
  min-width: max-content;
  padding: var(--button-padding, 4px);
  border-radius: var(--radius-full);
  border: none;
  transition: background-color 0.3s;
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;

  ::after {
    content: '';
    position: absolute;
    inset: 0;
    border-radius: var(--radius-full);
    pointer-events: none;
  }

  &.toggledOn {
    background-color: var(--color-menu-entry-selected-bg);
  }

  &:hover,
  &:focus,
  &:active {
    ::after {
      background-color: var(--color-menu-entry-hover-bg);
    }
  }
  &.disabled {
    cursor: default;
    opacity: 0.2;
    &:hover {
      background-color: unset;
    }
  }
}

.hoverArea {
  position: absolute;
  /*noinspection CssUnresolvedCustomProperty*/
  inset: calc(var(--extraClickZone, 0) * -1);
  cursor: pointer;
}
</style>
