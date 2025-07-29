<script setup lang="ts">
/** @file Provides a fullscreen mode to its slot, based on conditional teleport and conditional styling. */

import { useFullscreenAnimation } from '@/components/WithFullScreenMode/fullscreenAnimation'
import { registerHandlers, toggledAction } from '@/providers/action'
import { useFullscreenRoot } from '@/providers/fullscreenRoot'
import { providePopoverRoot, usePopoverRoot } from '@/providers/popoverRoot'
import { computed, useTemplateRef } from 'vue'

export type SavedSize = Keyframe

const fullscreen = defineModel<boolean>({ required: true })

const { enabled = true } = defineProps<{
  enabled?: boolean
}>()

/**
 * This value contains the non-fullscreen size of the element, stored for animating the return from fullscreen mode; the
 * presence or absence of the value is also used to determine whether the entering-fullscreen animation has already been
 * performed.
 *
 * The value is exposed to enable transferring state between `WithFullscreenMode` instances. One fullscreen component
 * can replace another by:
 * - The `savedSize` of the outgoing component is stored.
 * - The outgoing component is unmounted or hidden (so that it disappears without animation).
 * - The previously-stored `savedSize` is passed to the incoming component.
 * - The incoming component is displayed (the `savedSize` prevents any fullscreen animation).
 * - If the new component leaves fullscreen mode, its "return" to the dimensions of the original component will be
 *   animated.
 *
 * This approach was previously used when switching visualizations (when each visualization had its own
 * `VisualizationContainer` and `WithFullscreenMode` instance).
 */
const savedSize = defineModel<SavedSize | undefined>('savedSize')
const emit = defineEmits<{
  /**
   * This value is set to `true` for the duration of any entering-fullscreen or leaving-fullscreen animation. Until the
   * first event is received, it can be assumed to be `false`.
   *
   * Most layout changes that depend on whether the element is in fullscreen mode should treat the `animating` state as
   * equivalent to fullscreen. For example, if a button changes z-index to remain above the element when it is moved to
   * fullscreen, logic such as `{ aboveFullscreen: fullscreen || animating }` will ensure the button's appearance is
   * stable throughout the transition.
   */
  'update:animating': [boolean]
}>()

const fullscreenRoot = useFullscreenRoot()
const content = useTemplateRef('content')
const { animating } = useFullscreenAnimation({
  content,
  savedSize,
  emit,
  fullscreenRoot,
  fullscreen,
})
const active = computed(() => fullscreen.value || animating.value > 0)

const originalPopoverRoot = usePopoverRoot(true)
providePopoverRoot(
  computed(() => (active.value ? fullscreenRoot.value : originalPopoverRoot?.value)),
)

registerHandlers({
  'panel.fullscreen': {
    ...toggledAction(fullscreen),
    available: () => enabled,
    icon: () => (fullscreen.value ? 'exit_fullscreen' : 'fullscreen'),
    description: () => (fullscreen.value ? 'Exit Fullscreen' : 'Fullscreen'),
  },
})
</script>

<!-- The outer `div` is to avoid having a dynamic root. A component whose root may change cannot be passed to a `slot`,
or used with `unrefElement`. -->
<template>
  <div class="WithFullscreenMode fullsize">
    <Teleport :disabled="!active" :to="fullscreenRoot">
      <div ref="content" class="fullsize" :class="{ active }">
        <slot />
      </div>
    </Teleport>
  </div>
</template>

<style scoped>
.fullsize {
  width: 100%;
  height: 100%;
}

.active {
  position: absolute;
  z-index: 1;
}
</style>
