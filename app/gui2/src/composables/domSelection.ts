import { unrefElement, useEvent, useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import type { MaybeElement } from '@vueuse/core'
import { ref, watch, type Ref } from 'vue'

export function useSelectionBounds(boundingElement: Ref<MaybeElement>) {
  const bounds = ref<Rect>()

  function getSelectionBounds(selection: Selection, element: Element) {
    if (!selection.isCollapsed && element.contains(selection.anchorNode)) {
      const domRange = selection.getRangeAt(0)
      if (selection.anchorNode === element) {
        let inner = element
        while (inner.firstElementChild != null) {
          inner = inner.firstElementChild as HTMLElement
        }
        return Rect.FromDomRect(inner.getBoundingClientRect())
      } else {
        return Rect.FromDomRect(domRange.getBoundingClientRect())
      }
    } else {
      return undefined
    }
  }

  function update() {
    const selection = window.getSelection()
    const element = unrefElement(boundingElement)
    if (selection != null && element != null) {
      bounds.value = getSelectionBounds(selection, element)
    } else {
      bounds.value = undefined
    }
  }

  useEvent(document, 'selectionchange', update)

  const size = useResizeObserver(boundingElement)
  watch(size, update)

  return { bounds }
}
