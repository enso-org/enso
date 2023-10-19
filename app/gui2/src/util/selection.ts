import { selectionMouseBindings } from '@/bindings'
import type { Rect } from '@/stores/rect'
import { usePointer } from '@/util/events'
import type { NavigatorComposable } from '@/util/navigator'
import type { Vec2 } from '@/util/vec2'
import { computed, proxyRefs, reactive, shallowRef } from 'vue'

export type SelectionComposable<T> = ReturnType<typeof useSelection<T>>
export function useSelection<T>(
  navigator: NavigatorComposable,
  elementRects: Map<T, Rect>,
  margin: number,
  callbacks: {
    onSelected?: (element: T) => void
    onDeselected?: (element: T) => void
  } = {},
) {
  const anchor = shallowRef<Vec2>()
  const initiallySelected = new Set<T>()
  const selected = reactive(new Set<T>())

  function setSelection(newSelection: Set<T>) {
    for (const id of newSelection)
      if (!selected.has(id)) {
        selected.add(id)
        callbacks.onSelected?.(id)
      }
    for (const id of selected)
      if (!newSelection.has(id)) {
        selected.delete(id)
        callbacks.onDeselected?.(id)
      }
  }

  function execAdd() {
    setSelection(new Set([...initiallySelected, ...intersectingElements.value]))
  }

  function execRemove() {
    const newSelection = new Set([...initiallySelected])
    for (const t of intersectingElements.value) newSelection.delete(t)
    setSelection(newSelection)
  }

  const selectionEventHandler = selectionMouseBindings.handler({
    replace() {
      setSelection(intersectingElements.value)
    },
    add: execAdd,
    remove: execRemove,
    toggle() {
      const numCommon = countCommonInSets(initiallySelected, intersectingElements.value)
      const adding = numCommon * 2 <= intersectingElements.value.size
      if (adding) execAdd()
      else execRemove()
    },
    invert() {
      const newSelection = new Set(initiallySelected)
      for (const id of intersectingElements.value) {
        if (initiallySelected.has(id)) newSelection.delete(id)
        else newSelection.add(id)
      }
      setSelection(newSelection)
    },
  })

  const intersectingElements = computed<Set<T>>(() => {
    if (!pointer.dragging || anchor.value == null || navigator.sceneMousePos == null) {
      return new Set()
    }
    const navigatorSpaceMargin = margin / navigator.scale

    const a = navigator.sceneMousePos
    const b = anchor.value

    const left = Math.min(a.x, b.x) - navigatorSpaceMargin
    const right = Math.max(a.x, b.x) + navigatorSpaceMargin
    const top = Math.min(a.y, b.y) - navigatorSpaceMargin
    const bottom = Math.max(a.y, b.y) + navigatorSpaceMargin
    const intersectingElements = new Set<T>()
    for (const [id, rect] of elementRects) {
      const rectLeft = rect.pos.x
      const rectRight = rectLeft + rect.size.x
      const rectTop = rect.pos.y
      const rectBottom = rectTop + rect.size.y
      if (left <= rectRight && right >= rectLeft && top <= rectBottom && bottom >= rectTop) {
        intersectingElements.add(id)
      }
    }
    return intersectingElements
  })

  const pointer = usePointer((pos, event, eventType) => {
    if (eventType === 'start') {
      initiallySelected.clear()
    } else if (pointer.dragging && anchor.value == null) {
      anchor.value = navigator.sceneMousePos?.copy()
    } else if (eventType === 'stop') {
      anchor.value = undefined
      initiallySelected.clear()
    }
    selectionEventHandler(event)
  })
  return proxyRefs({
    selected,
    anchor,
    selectAll: () => {
      for (const id of elementRects.keys()) selected.add(id)
    },
    deselectAll: () => selected.clear(),
    isSelected: (element: T) => selected.has(element),
    mouseHandler: selectionEventHandler,
    events: pointer.events,
  })
}

function countCommonInSets(a: Set<unknown>, b: Set<unknown>): number {
  let count = 0
  for (const item in a) count += +b.has(item)
  return count
}
