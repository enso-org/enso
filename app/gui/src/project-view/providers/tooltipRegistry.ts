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

export interface HoveredElement {
  element: HTMLElement
  entry: TooltipEntry
}

export type TooltipRegistry = ReturnType<typeof useTooltipRegistry>
export const [provideTooltipRegistry, useTooltipRegistry] = createContextStore(
  'tooltip registry',
  () => {
    type EntriesSet = ShallowReactive<Set<TooltipEntry>>
    const hoveredElements = shallowReactive<Map<HTMLElement, EntriesSet>>(new Map())

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
      registerTooltip(slot: Ref<Slot | undefined>) {
        const key = Symbol()
        const registeredElements = new Set<HTMLElement>()
        onUnmounted(() => {
          for (const el of registeredElements) {
            methods.onTargetLeave(el)
          }
        })

        const methods = {
          onTargetEnter(target: HTMLElement) {
            const entriesSet: EntriesSet = hoveredElements.get(target) ?? shallowReactive(new Set())
            entriesSet.add({ contents: slot, isHidden: false, key })
            // make sure that the newly entered target is on top of the map
            hoveredElements.delete(target)
            hoveredElements.set(target, entriesSet)
            registeredElements.add(target)
          },
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
