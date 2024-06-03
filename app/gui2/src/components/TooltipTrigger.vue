<script setup lang="ts">
import { usePropagateScopesToAllRoots } from '@/util/patching'
import { debouncedGetter } from '@/util/reactivity'
import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/vue'
import { computed, ref, useSlots, watchEffect } from 'vue'

// Time for which hover must remain on a single element for tooltip to show up.
const TOOLTIP_SHOW_DELAY_MS = 1000
// Time after which tooltip will disappear once an element is no longer hovered.
const TOOLTIP_HIDE_DELAY_MS = 500

usePropagateScopesToAllRoots()

const hoveredElements = ref<Array<HTMLElement>>([])
const lastHovered = computed(() => hoveredElements.value[hoveredElements.value.length - 1])
const lastHoveredDebounceShow = debouncedGetter(() => lastHovered.value, TOOLTIP_SHOW_DELAY_MS)
const lastHoveredDebounceHide = debouncedGetter(() => lastHovered.value, TOOLTIP_HIDE_DELAY_MS)
const floatTarget = computed(() => {
  const show = lastHoveredDebounceShow.value
  const hide = lastHoveredDebounceHide.value
  return show === hide ? show : undefined
})
const showPotentialAnimations = computed(
  () => lastHoveredDebounceShow.value != null || lastHoveredDebounceHide.value != null,
)

const tooltip = ref<HTMLDivElement>()
const floating = useFloating(floatTarget, tooltip, {
  placement: 'top',
  transform: false,
  middleware: [offset(5), flip(), shift()],
  whileElementsMounted: autoUpdate,
})

watchEffect(() => {
  console.log('floating', floating.floatingStyles.value)
})

function clearRemovedHovers() {
  const filtered = hoveredElements.value.filter((e) => e.isConnected)
  // Only update array ref if we removed anything.
  if (filtered.length < hoveredElements.value.length) hoveredElements.value = filtered
}

function onEnter(e: PointerEvent) {
  if (e.target instanceof HTMLElement) {
    hoveredElements.value.push(e.target)
    clearRemovedHovers()
  }
}

function onLeave(e: PointerEvent) {
  if (e.target instanceof HTMLElement) {
    const i = hoveredElements.value.indexOf(e.target)
    if (i >= 0) hoveredElements.value.splice(i, 1)
    clearRemovedHovers()
  }
}
</script>

<template>
  <slot v-bind="{ ...$attrs }" @pointerenter="onEnter" @pointerleave="onLeave" />
  <Teleport v-if="showPotentialAnimations" to="body">
    <TransitionGroup>
      <div
        v-if="floatTarget != null"
        ref="tooltip"
        class="Tooltip"
        :style="floating.floatingStyles.value"
      >
        <slot name="tooltip" />
      </div>
    </TransitionGroup>
  </Teleport>
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
