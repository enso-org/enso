import { useEvent } from '@/composables/events'
import type { Opt } from 'shared/util/data/opt'
import { watchEffect, type Ref } from 'vue'

/** Automatically `blur` the currently active element on any mouse click outside of `root`.
 * It is useful when other elements may capture pointer events, preventing default browser behavior for focus change. */
export function useAutoBlur(root: Ref<HTMLElement | SVGElement | undefined>) {
  watchEffect((onCleanup) => {
    const element = root.value
    if (element) {
      autoBlurRoots.add(element)
      onCleanup(() => autoBlurRoots.delete(element))
    }
  })
}

const autoBlurRoots = new Set<HTMLElement | SVGElement | MathMLElement>()

export function registerAutoBlurHandler() {
  useEvent(
    window,
    'pointerdown',
    (event) => {
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
}

/** Returns true if the target of the event is outside the DOM subtree of the given `area` element. */
export function targetIsOutside(e: Event, area: Opt<Element>): boolean {
  return !!area && e.target instanceof Element && !area.contains(e.target)
}
