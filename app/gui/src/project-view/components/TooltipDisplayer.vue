<script setup lang="ts">
import { HoveredElement, type TooltipRegistry } from '@/providers/tooltipRegistry'
import { autoUpdate, flip, FloatingElement, offset, shift, useFloating } from '@floating-ui/vue'
import { computed, ref, shallowRef, toValue, watch } from 'vue'

const props = defineProps<{ registry: TooltipRegistry }>()

// Time for which hover must remain on a single element for tooltip to show up.
const TOOLTIP_SHOW_DELAY_MS = 1500
// Time after which tooltip will disappear once an element is no longer hovered.
const TOOLTIP_HIDE_DELAY_MS = 300

// Currently hovered element and its tooltip entry.
const activeTooltip = props.registry.lastHoveredElement
// Previously hovered element and its tooltip entry.
const previousTooltip = shallowRef<HoveredElement>()
const show = ref(false)

type Timeout = ReturnType<typeof setTimeout> | null
let hideTimeout: Timeout
let showTimeout: Timeout

function resetTimeout(timeout: Timeout) {
  if (timeout != null) {
    clearTimeout(timeout)
    timeout = null
  }
}

watch(activeTooltip, (newValue, oldValue) => {
  if (oldValue == null && newValue != null) {
    // We are hovering some element, show its tooltip after delay.
    resetTimeout(showTimeout)
    resetTimeout(hideTimeout)
    showTimeout = setTimeout(() => {
      show.value = true
      showTimeout = null
    }, TOOLTIP_SHOW_DELAY_MS)
    previousTooltip.value = toValue(newValue)
  } else if (oldValue != null && newValue == null) {
    // We no longer hover any element, hide the tooltip after delay.
    resetTimeout(showTimeout)
    resetTimeout(hideTimeout)
    hideTimeout = setTimeout(() => {
      show.value = false
      hideTimeout = null
    }, TOOLTIP_HIDE_DELAY_MS)
  }
  // There is no need to check if both `oldValue` and `newValue` are not null, because
  // `activeTooltip` is always set to null intermitently when switching between elements.
})

const displayedTooltip = computed(() => {
  if (!show.value) return undefined
  // When hovering the element, display its tooltip.
  if (
    activeTooltip.value != null &&
    !activeTooltip.value.entry.isHidden &&
    activeTooltip.value.element.isConnected
  ) {
    return activeTooltip.value
  }
  // If no element with the tooltip is being hovered, display the tooltip of the previously hovered element.
  // (until it will be hidden after timeout, by changing the `show` ref)
  if (
    previousTooltip.value != null &&
    !previousTooltip.value.entry.isHidden &&
    previousTooltip.value.element.isConnected
  ) {
    return previousTooltip.value
  }
  return undefined
})
const floatTarget = computed(() => displayedTooltip.value?.element)
const tooltipContents = computed(() => displayedTooltip.value?.entry.contents.value)

// Default implementation of autoUpdate ignores cases when the reference is being unmounted while in animation.
// This is a hotfix for that.
function whileElementsMounted(
  reference: HTMLElement,
  floating: FloatingElement,
  update: () => void,
): () => void {
  return autoUpdate(reference, floating, () => {
    if (reference.isConnected) {
      update()
    }
  })
}

const tooltip = ref<HTMLDivElement>()
const floating = useFloating(floatTarget, tooltip, {
  placement: 'top',
  transform: false,
  middleware: [offset(5), flip(), shift()],
  whileElementsMounted,
})
</script>

<template>
  <Transition>
    <div
      v-if="floatTarget != null && tooltipContents != null"
      :key="displayedTooltip?.entry.key ?? 0"
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
