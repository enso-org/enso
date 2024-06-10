import { createContextStore } from '@/providers'
import { last } from '@/util/data/iterable'
import {
  computed,
  onUnmounted,
  shallowReactive,
  type Ref,
  type ShallowReactive,
  type Slot,
} from 'vue'

interface TooltipEntry {
  contents: Ref<Slot | undefined>
  key: symbol
}

export type TooltipRegistry = ReturnType<typeof injectFn>
export { provideFn as provideTooltipRegistry, injectFn as useTooltipRegistry }
const { provideFn, injectFn } = createContextStore('tooltip registry', () => {
  type EntriesSet = ShallowReactive<Set<TooltipEntry>>
  const hoveredElements = shallowReactive<Map<HTMLElement, EntriesSet>>(new Map())

  const lastHoveredElement = computed(() => {
    return last(hoveredElements.keys())
  })

  return {
    lastHoveredElement,
    getElementEntry(el: HTMLElement | undefined): TooltipEntry | undefined {
      const set = el && hoveredElements.get(el)
      return set ? last(set) : undefined
    },
    registerTooltip(slot: Ref<Slot | undefined>) {
      const entry: TooltipEntry = {
        contents: slot,
        key: Symbol(),
      }
      const registeredElements = new Set<HTMLElement>()
      onUnmounted(() => {
        for (const el of registeredElements) {
          methods.onTargetLeave(el)
        }
      })

      const methods = {
        onTargetEnter(target: HTMLElement) {
          const entriesSet: EntriesSet = hoveredElements.get(target) ?? shallowReactive(new Set())
          entriesSet.add(entry)
          // make sure that the newly entered target is on top of the map
          hoveredElements.delete(target)
          hoveredElements.set(target, entriesSet)
          registeredElements.add(target)
        },
        onTargetLeave(target: HTMLElement) {
          const entriesSet = hoveredElements.get(target)
          entriesSet?.delete(entry)
          registeredElements.delete(target)
          if (entriesSet?.size === 0) {
            hoveredElements.delete(target)
          }
        },
      }
      return methods
    },
  }
})
