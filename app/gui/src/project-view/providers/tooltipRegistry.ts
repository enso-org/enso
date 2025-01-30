import { createContextStore } from '@/providers'
import * as iter from 'enso-common/src/utilities/data/iter'
import {
  computed,
  onUnmounted,
  shallowReactive,
  type Ref,
  type ShallowReactive,
  type Slot,
} from 'vue'
import { assert } from 'ydoc-shared/util/assert'

interface TooltipEntry {
  contents: Ref<Slot | undefined>
  isHidden: boolean
  key: symbol
}

/** A hovered element with its corresponding tooltip. */
export interface HoveredElement {
  element: HTMLElement
  entry: TooltipEntry
}

export type TooltipRegistry = ReturnType<typeof useTooltipRegistry>
export const [provideTooltipRegistry, useTooltipRegistry] = createContextStore(
  'tooltip registry',
  () => {
    type EntriesSet = ShallowReactive<Set<TooltipEntry>>
    // A map of hovered elements to their corresponding tooltips.
    // There can be multiple tooltips for the same element, because we can have
    // multiple nested tooltip triggers (components calling `registerTooltip`).
    // The last hovered element is always on top of the map.
    const hoveredElements = shallowReactive<Map<HTMLElement, EntriesSet>>(new Map())

    /** The last hovered element with its corresponding tooltip. Undefined if no element with tooltip is hovered. */
    const lastHoveredElement = computed<HoveredElement | undefined>(() => {
      const lastKey = iter.last(hoveredElements.keys())
      if (lastKey == null) return undefined
      const entries = hoveredElements.get(lastKey)
      assert(entries != null, 'entries is never null if lastKey is not null')
      const lastEntry = iter.last(entries)
      if (lastEntry == null) return undefined
      return { element: lastKey, entry: lastEntry }
    })

    return {
      lastHoveredElement,
      /** Registers a tooltip and returns methods to control it. See `TooltipTrigger` component for usage. */
      registerTooltip(slot: Ref<Slot | undefined>) {
        const key = Symbol()
        const registeredElements = new Set<HTMLElement>()
        onUnmounted(() => {
          for (const el of registeredElements) {
            methods.onTargetLeave(el)
          }
        })

        const methods = {
          /** The registered tooltip must be shown when hovering this element. */
          onTargetEnter(target: HTMLElement) {
            const entriesSet: EntriesSet = hoveredElements.get(target) ?? shallowReactive(new Set())
            entriesSet.add({ contents: slot, isHidden: false, key })
            // make sure that the newly entered target is on top of the map
            hoveredElements.delete(target)
            hoveredElements.set(target, entriesSet)
            registeredElements.add(target)
          },
          /** The registered tooltip must be hidden when finishing hovering this element. */
          onTargetLeave(target: HTMLElement) {
            const entriesSet = hoveredElements.get(target)
            if (entriesSet) {
              for (const e of entriesSet) {
                if (e.key === key) entriesSet.delete(e)
              }
            }
            registeredElements.delete(target)
            if (entriesSet?.size === 0) {
              hoveredElements.delete(target)
            }
          },
          /**
           * Forcefully hides the registered tooltip.
           * Useful when we need to hide the tooltip without moving the mouse out of the element,
           * like when clicking on a button.
           *
           * If several tooltips are registered for the same element, all of them will hide.
           */
          forceHide() {
            for (const el of registeredElements) {
              const entriesSet = hoveredElements.get(el)
              const newSet = new Set(entriesSet)
              newSet.forEach((entry) => (entry.isHidden = true))
              hoveredElements.set(el, newSet)
            }
          },
        }
        return methods
      },
    }
  },
)
