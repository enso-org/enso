/** @file A Vue composable for keeping track of selected DOM elements. */

import { selectionMouseBindings } from '@/bindings'
import { useEvent, usePointer } from '@/composables/events'
import type { NavigatorComposable } from '@/composables/navigator'
import type { PortId } from '@/providers/portInfo.ts'
import { type NodeId } from '@/stores/graph'
import type { Rect } from '@/util/data/rect'
import type { Vec2 } from '@/util/data/vec2'
import { computed, proxyRefs, reactive, ref, shallowRef } from 'vue'

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
  const hoveredNode = ref<NodeId>()
  const hoveredPort = ref<PortId>()

  useEvent(document, 'pointerover', (event) => {
    if (event.target instanceof Element) {
      const widgetPort = event.target.closest('.WidgetPort')
      hoveredPort.value =
        widgetPort instanceof HTMLElement &&
        'port' in widgetPort.dataset &&
        typeof widgetPort.dataset.port === 'string'
          ? (widgetPort.dataset.port as PortId)
          : undefined
    }
  })

  function readInitiallySelected() {
    initiallySelected.clear()
    for (const id of selected) initiallySelected.add(id)
  }

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
    setSelection(new Set([...initiallySelected, ...elementsToSelect.value]))
  }

  function execRemove() {
    const newSelection = new Set([...initiallySelected])
    for (const t of elementsToSelect.value) newSelection.delete(t)
    setSelection(newSelection)
  }

  const selectionEventHandler = selectionMouseBindings.handler({
    replace() {
      setSelection(elementsToSelect.value)
    },
    add: execAdd,
    remove: execRemove,
    toggle() {
      const numCommon = countCommonInSets(initiallySelected, elementsToSelect.value)
      const adding = numCommon * 2 <= elementsToSelect.value.size
      if (adding) execAdd()
      else execRemove()
    },
    invert() {
      const newSelection = new Set(initiallySelected)
      for (const id of elementsToSelect.value) {
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

  const overrideElemsToSelect = ref<Set<T>>()
  const elementsToSelect = computed(() => overrideElemsToSelect.value ?? intersectingElements.value)

  function handleSelectionOf(event: MouseEvent, elements: Set<T>) {
    readInitiallySelected()
    overrideElemsToSelect.value = elements
    selectionEventHandler(event)
    overrideElemsToSelect.value = undefined
  }

  const pointer = usePointer((_pos, event, eventType) => {
    if (eventType === 'start') {
      readInitiallySelected()
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
    setSelection,
    handleSelectionOf,
    hoveredNode,
    hoveredPort,
    mouseHandler: selectionEventHandler,
    events: pointer.events,
  })
}

function countCommonInSets(a: Set<unknown>, b: Set<unknown>): number {
  let count = 0
  for (const item in a) count += +b.has(item)
  return count
}
