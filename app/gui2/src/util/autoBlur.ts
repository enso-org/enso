import { unrefElement, useEvent } from '@/composables/events'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { VueInstance } from '@vueuse/core'
import { watchEffect, type Ref } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'

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
  return isNodeOutside(e.target, area)
}

/** Returns true if the `element` argument is a node outside the DOM subtree of the given `area`. */
export function isNodeOutside(element: any, area: Opt<Node>): boolean {
  return !!area && element instanceof Node && !area.contains(element)
}

/** Returns a new interaction based on the given `interaction`. The new interaction will be ended if a pointerdown event
 *  occurs outside the given `area` element. */
export function endOnClickOutside(
  area: Ref<Element | VueInstance | null | undefined>,
  interaction: Interaction,
): Interaction {
  return endOnClick((e: PointerEvent) => targetIsOutside(e, unrefElement(area)), interaction) 
}

export function endOnClick(
  condition: (e: PointerEvent) => boolean,
  interaction: Interaction,
): Interaction {
  const chainedPointerdown = interaction.pointerdown
  const handler = injectInteractionHandler()
  const wrappedInteraction: Interaction = {
    ...interaction,
    pointerdown: (e: PointerEvent, ...args) => {
      const shouldEnd = condition(e)
      if (shouldEnd) {
        handler.end(wrappedInteraction)
        return false
      }
      return chainedPointerdown ? chainedPointerdown(e, ...args) : false
    },
  }
  return wrappedInteraction
}
