<script setup lang="ts">
import { type TooltipRegistry } from '@/providers/tooltipRegistry'
import { debouncedGetter } from '@/util/reactivity'
import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/vue'
import { computed, ref } from 'vue'

const props = defineProps<{ registry: TooltipRegistry }>()

// Time for which hover must remain on a single element for tooltip to show up.
const TOOLTIP_SHOW_DELAY_MS = 1500
// Time after which tooltip will disappear once an element is no longer hovered.
const TOOLTIP_HIDE_DELAY_MS = 300

const activeTooltip = computed(() => {
  const element = props.registry.lastHoveredElement.value
  return { element, entry: props.registry.getElementEntry(element) }
})
const doShow = debouncedGetter(
  () => activeTooltip.value.element != null && activeTooltip.value.entry != null,
  TOOLTIP_SHOW_DELAY_MS,
)
const displayedEntry = debouncedGetter(activeTooltip, TOOLTIP_HIDE_DELAY_MS)
const floatTarget = computed(() => {
  return doShow.value && displayedEntry.value.element?.isConnected ?
      displayedEntry.value.element
    : undefined
})

const tooltip = ref<HTMLDivElement>()
const floating = useFloating(floatTarget, tooltip, {
  placement: 'top',
  transform: false,
  middleware: [offset(5), flip(), shift()],
  whileElementsMounted: autoUpdate,
})

const tooltipContents = computed(() => displayedEntry.value.entry?.contents.value)
</script>

<template>
  <Transition>
    <div
      v-if="floatTarget != null && tooltipContents != null"
      :key="displayedEntry.entry?.key ?? 0"
      ref="tooltip"
      class="Tooltip"
      :style="floating.floatingStyles.value"
    >
      <component :is="tooltipContents" />
    </div>
  </Transition>
</template>

<style scoped>
.Tooltip {
  position: absolute;
  font-family: var(--font-sans);
  font-size: 11.5px;
  min-height: 32px;
  line-height: 20px;
  background: rgba(0 0 0 / 80%);
  color: rgba(255 255 255 / 80%);
  padding: 4px 8px;
  border-radius: 16px;
  pointer-events: none;

  display: flex;
  justify-content: center;
  align-items: center;
  text-align: center;
  text-wrap: balance;

  &.v-enter-active,
  &.v-leave-active {
    transition:
      transform 0.1s ease-out,
      opacity 0.1s ease-out;
  }

  &.v-enter-from,
  &.v-leave-to {
    transform: translateY(-2px);
    opacity: 0;
  }
}
</style>
