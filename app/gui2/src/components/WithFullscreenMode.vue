<script setup lang="ts">
/** @file Provides a fullscreen mode to its slot, based on conditional teleport and conditional styling. */

import { useFullscreenContext } from '@/providers/fullscreenContext'
import { Rect } from '@/util/data/rect'
import { computed, ref, toRef, watch } from 'vue'

const props = defineProps<{
  fullscreen: boolean
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

const content = ref<HTMLElement>()

const { fullscreenContainer } = useFullscreenContext()

const fullscreenSize: Keyframe = {
  top: 0,
  left: 0,
  height: '100%',
  width: '100%',
}

const animating = ref(0)

watch(animating, (value, oldValue) => {
  if (value && !oldValue) emit('update:animating', true)
  else if (!value && oldValue) emit('update:animating', false)
})

function animate(start: Keyframe, end: Keyframe) {
  const el = content.value
  if (!el) return
  animating.value += 1
  el.animate([start, end], { duration: 200, easing: 'ease-in-out' }).finished.then(
    () => (animating.value -= 1),
  )
}

watch(
  [toRef(props, 'fullscreen'), content, fullscreenContainer],
  ([fullscreen, el, fullscreenContainer]) => {
    if (!el || !fullscreenContainer) return
    const container = fullscreenContainer.getBoundingClientRect()
    if (fullscreen && !savedSize.value) {
      const inner = Rect.FromDomRect(el.getBoundingClientRect())
      const startSize = {
        top: `${inner.top - container.top}px`,
        left: `${inner.left - container.left}px`,
        height: `${inner.height}px`,
        width: `${inner.width}px`,
      }
      animate(startSize, fullscreenSize)
      savedSize.value = startSize
    } else if (!fullscreen && savedSize.value) {
      animate(fullscreenSize, savedSize.value)
      savedSize.value = undefined
    }
  },
)

const active = computed(() => props.fullscreen || animating.value)
</script>

<script lang="ts">
export type SavedSize = Keyframe
</script>

<!-- The outer `div` is to avoid having a dynamic root. A component whose root may change cannot be passed to a `slot`,
or used with `unrefElement`. -->
<template>
  <div class="WithFullscreenMode fullsize">
    <Teleport defer :disabled="!active" :to="fullscreenContainer">
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
