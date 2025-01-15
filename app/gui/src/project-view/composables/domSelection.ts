import { unrefElement, useEvent, useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import type { MaybeElement } from '@vueuse/core'
import { shallowRef, watch, type Ref } from 'vue'

/** TODO: Add docs */
export function useSelectionBounds(boundingElement: Ref<MaybeElement>, includeCollapsed = false) {
  const bounds = shallowRef<Rect>()
  const collapsed = shallowRef<boolean>()

  function getSelectionBounds(selection: Selection, element: Element) {
    if ((includeCollapsed || !selection.isCollapsed) && element.contains(selection.anchorNode)) {
      if (selection.anchorNode === element) {
        let inner = element
        while (inner.firstElementChild != null) {
          inner = inner.firstElementChild as HTMLElement
        }
        return Rect.FromDomRect(inner.getBoundingClientRect())
      } else if (selection.isCollapsed && selection.anchorNode) {
        const element =
          selection.anchorNode instanceof Element ?
            selection.anchorNode
          : selection.anchorNode.parentElement
        if (element) return Rect.FromDomRect(element.getBoundingClientRect())
      }

      const domRange = selection.getRangeAt(0)
      return Rect.FromDomRect(domRange.getBoundingClientRect())
    } else {
      return undefined
    }
  }

  function update() {
    const selection = window.getSelection()
    const element = unrefElement(boundingElement)
    collapsed.value = selection?.isCollapsed
    if (selection != null && element != null) {
      bounds.value = getSelectionBounds(selection, element)
    } else {
      bounds.value = undefined
    }
  }

  useEvent(document, 'selectionchange', update)

  const size = useResizeObserver(boundingElement)
  watch(size, update)

  return { bounds, collapsed }
}
