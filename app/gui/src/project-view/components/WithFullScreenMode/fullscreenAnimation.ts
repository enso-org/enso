import { unrefElement } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { ref, watch, type Ref, type WatchSource } from 'vue'

interface Emit {
  (event: 'update:animating', animating: boolean): void
}

interface FullscreenAnimationOptions {
  content: Parameters<typeof unrefElement>[0]
  savedSize: Ref<Keyframe | undefined>
  fullscreenRoot: Parameters<typeof unrefElement>[0]
  fullscreen: WatchSource<boolean>
  emit: Emit
}

const fullscreenSize: Keyframe = {
  top: 0,
  left: 0,
  height: '100%',
  width: '100%',
}

/**
 * Animates the transition between rendering in fullscreen mode and within the bounds of another
 * element.
 */
export function useFullscreenAnimation({
  content,
  savedSize,
  emit,
  fullscreenRoot,
  fullscreen,
}: FullscreenAnimationOptions) {
  const animating = ref(0)

  watch(animating, (value, oldValue) => {
    if (value && !oldValue) emit('update:animating', true)
    else if (!value && oldValue) emit('update:animating', false)
  })

  function animate(start: Keyframe, end: Keyframe) {
    const el = unrefElement(content)
    if (!el) return
    animating.value += 1
    el.animate([start, end], { duration: 200, easing: 'ease-in-out' }).finished.then(
      () => (animating.value -= 1),
    )
  }

  watch([fullscreen, content], ([fullscreen, content]) => {
    const fullscreenContainer = unrefElement(fullscreenRoot)
    const el = unrefElement(content)
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
  })

  return { animating }
}
