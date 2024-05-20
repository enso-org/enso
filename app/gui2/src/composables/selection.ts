/** @file A Vue composable for keeping track of selected DOM elements. */
import { selectionMouseBindings } from '@/bindings'
import { useEvent, usePointer } from '@/composables/events'
import type { PortId } from '@/providers/portInfo.ts'
import { type NodeId } from '@/stores/graph'
import type { Rect } from '@/util/data/rect'
import { intersectionSize } from '@/util/data/set'
import type { Vec2 } from '@/util/data/vec2'
import { dataAttribute, elementHierarchy } from '@/util/dom'
import * as set from 'lib0/set'
import { filter } from 'shared/util/data/iterable'
import { computed, ref, shallowReactive, shallowRef } from 'vue'

export function useSelection<T>(
  navigator: { sceneMousePos: Vec2 | null; scale: number },
  elementRects: Map<T, Rect>,
  margin: number,
  isValid: (element: T) => boolean,
  callbacks: {
    onSelected?: (element: T) => void
    onDeselected?: (element: T) => void
  } = {},
) {
  const anchor = shallowRef<Vec2>()
  let initiallySelected = new Set<T>()
  // Selection, including elements that do not (currently) pass `isValid`.
  const rawSelected = shallowReactive(new Set<T>())

  const selected = computed(() => set.from(filter(rawSelected, isValid)))
  const isChanging = computed(() => anchor.value != null)
  const committedSelection = computed(() =>
    isChanging.value ? set.from(filter(initiallySelected, isValid)) : selected.value,
  )

  function readInitiallySelected() {
    initiallySelected = set.from(rawSelected)
  }

  function setSelection(newSelection: Set<T>) {
    for (const id of newSelection)
      if (!rawSelected.has(id)) {
        rawSelected.add(id)
        callbacks.onSelected?.(id)
      }
    for (const id of rawSelected)
      if (!newSelection.has(id)) {
        rawSelected.delete(id)
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
      const numCommon = intersectionSize(initiallySelected, elementsToSelect.value)
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

  const pointer = usePointer(
    (_pos, event, eventType) => {
      if (eventType === 'start') {
        readInitiallySelected()
      } else if (eventType === 'stop') {
        if (anchor.value == null) {
          // If there was no drag, we want to handle "clicking-off" selected nodes.
          selectionEventHandler(event)
        } else {
          anchor.value = undefined
        }
        initiallySelected.clear()
      } else if (pointer.dragging) {
        if (anchor.value == null) {
          anchor.value = navigator.sceneMousePos?.copy()
        }
        selectionEventHandler(event)
      }
    },
    { predicate: (e) => e.target === e.currentTarget },
  )

  return {
    // === Selected nodes ===
    selected,
    selectAll: () => {
      for (const id of elementRects.keys()) rawSelected.add(id)
    },
    deselectAll: () => rawSelected.clear(),
    isSelected: (element: T) => rawSelected.has(element),
    committedSelection,
    setSelection,
    // === Selection changes ===
    anchor,
    isChanging,
    // === Mouse events ===
    handleSelectionOf,
    events: pointer.events,
  }
}

// === Hover tracking for nodes and ports ===

export function useGraphHover(isPortEnabled: (port: PortId) => boolean) {
  const hoveredElement = ref<Element>()

  useEvent(document, 'pointerover', (event) => {
    hoveredElement.value = event.target instanceof Element ? event.target : undefined
  })

  const hoveredPort = computed<PortId | undefined>(() => {
    if (!hoveredElement.value) return undefined
    for (const element of elementHierarchy(hoveredElement.value, '.WidgetPort')) {
      const portId = dataAttribute<PortId>(element, 'port')
      if (portId && isPortEnabled(portId)) return portId
    }
    return undefined
  })

  const hoveredNode = computed<NodeId | undefined>(() => {
    const element = hoveredElement.value?.closest('.GraphNode')
    if (!element) return undefined
    return dataAttribute<NodeId>(element, 'node')
  })

  return { hoveredNode, hoveredPort }
}
