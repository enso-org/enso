import { unrefElement, useEvent } from '@/composables/events'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { ToValue } from '@/util/reactivity'
import type { VueInstance } from '@vueuse/core'
import { toValue, watchEffect, type Ref } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'

/**
 * Automatically `blur` the currently active element on any mouse click outside of `root`.
 * It is useful when other elements may capture pointer events, preventing default browser behavior for focus change.
 */
export function useAutoBlur(root: ToValue<HTMLElement | SVGElement | undefined>) {
  watchEffect((onCleanup) => {
    const element = toValue(root)
    if (element) {
      autoBlurRoots.add(element)
      onCleanup(() => autoBlurRoots.delete(element))
    }
  })
}

const autoBlurRoots = new Set<HTMLElement | SVGElement | MathMLElement>()

/**
 * This function should be called on application mount to make all {@link useAutoBlur} work
 * properly.
 */
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

/**
 * Returns a new interaction based on the given `interaction`. The new interaction will be ended if a pointerdown event
 *  occurs outside the given `area` element.
 *
 * See also {@link cancelOnClickOutside}.
 */
export function endOnClickOutside(
  area: Ref<Opt<Element | VueInstance>>,
  interaction: Interaction,
): Interaction {
  return endOnClick(isClickOutside(area), interaction)
}

/**
 * Returns a new interaction based on the given `interaction`. The new interaction will be ended if a pointerdown event
 *  occurs such as a `condition` returns `true`.
 *
 * See also {@link cancelOnClickOutside}.
 */
export function endOnClick(
  condition: (e: PointerEvent) => boolean,
  interaction: Interaction,
): Interaction {
  const handler = injectInteractionHandler()
  return handleClick(condition, interaction, handler.end.bind(handler))
}

/**
 * Returns a new interaction based on the given `interaction`. The new interaction will be canceled if a pointerdown event
 *  occurs such as a `condition` returns `true`.
 *
 * See also {@link cancelOnClickOutside}.
 */
export function cancelOnClick(
  condition: (e: PointerEvent) => boolean,
  interaction: Interaction,
): Interaction {
  const handler = injectInteractionHandler()
  return handleClick(condition, interaction, handler.cancel.bind(handler))
}

/**
 * Returns a new interaction based on the given `interaction`. The new interaction will be canceled if a pointerdown event
 *  occurs outside the given `area` element.
 *
 * See also {@link endOnClickOutside}.
 */
export function cancelOnClickOutside(
  area: Ref<Opt<Element | VueInstance>>,
  interaction: Interaction,
) {
  const handler = injectInteractionHandler()
  return handleClick(isClickOutside(area), interaction, handler.cancel.bind(handler))
}

function isClickOutside(area: Ref<Opt<Element | VueInstance>>) {
  return (e: PointerEvent) => targetIsOutside(e, unrefElement(area))
}

/** Common part of {@link cancelOnClickOutside} and {@link endOnClickOutside}. */
function handleClick(
  condition: (e: PointerEvent) => boolean,
  interaction: Interaction,
  handler: (interaction: Interaction) => void,
) {
  const chainedPointerdown = interaction.pointerdown
  const wrappedInteraction: Interaction = {
    ...interaction,
    pointerdown: (e: PointerEvent) => {
      if (condition(e)) {
        handler(wrappedInteraction)
        return false
      }
      return chainedPointerdown ? chainedPointerdown(e) : false
    },
  }
  return wrappedInteraction
}
