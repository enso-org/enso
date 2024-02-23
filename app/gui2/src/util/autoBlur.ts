import { useEvent } from '@/composables/events'
import { watchEffect, type Ref } from 'vue'

/** Automatically `blur` the currently active element on any mouse click outside of `root`.
 * It is useful when other elements may capture pointer events, preventing default browser behavior for focus change. */
export function useAutoBlur(root: Ref<HTMLElement | SVGElement | undefined>) {
  watchEffect((onCleanup) => {
    const element = root.value
    if (element) {
      console.log('autoBlurRoots')
      autoBlurRoots.add(element)
      onCleanup(() => autoBlurRoots.delete(element))
    }
  })
}

const autoBlurRoots = new Set<HTMLElement | SVGElement | MathMLElement>()

useEvent(
  window,
  'pointerdown',
  (event) => {
    console.log('autoBlur')
    if (
      !(event.target instanceof Element) ||
      (!(document.activeElement instanceof HTMLElement) &&
        !(document.activeElement instanceof SVGElement) &&
        !(document.activeElement instanceof MathMLElement))
    )
      return false

    for (const root of autoBlurRoots) {
      if (root.contains(document.activeElement) && !root.contains(event.target)) {
        document.activeElement.blur()
        return true
      }
    }
    return false
  },
  { capture: true },
)
