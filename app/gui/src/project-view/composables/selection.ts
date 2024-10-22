/** @file A Vue composable for keeping track of selected DOM elements. */
import { selectionMouseBindings } from '@/bindings'
import { useEvent } from '@/composables/events'
import type { PortId } from '@/providers/portInfo.ts'
import { type NodeId } from '@/stores/graph'
import { filter, filterDefined, map } from '@/util/data/iterable'
import type { Rect } from '@/util/data/rect'
import { intersectionSize } from '@/util/data/set'
import { Vec2 } from '@/util/data/vec2'
import { dataAttribute, elementHierarchy } from '@/util/dom'
import * as set from 'lib0/set'
import { computed, ref, shallowReactive, shallowRef } from 'vue'
import { Err, Ok, type Result } from 'ydoc-shared/util/data/result'
import { NavigatorComposable } from './navigator'

interface BaseSelectionOptions<T> {
  margin?: number
  isValid?: (element: T) => boolean
  onSelected?: (element: T) => void
  onDeselected?: (element: T) => void
}
interface SelectionPackingOptions<T, PackedT> {
  /**
   * The `pack` and `unpack` functions are used to maintain state in a transformed form.
   *
   * If provided, all operations that modify or query state will transparently operate on packed state. This can be
   * used to expose a selection interface based on one element type (`T`), while allowing the selection set to be
   * maintained using a more stable element type (`PackedT`).
   *
   * For example, the selection can expose a `NodeId` API, while internally storing `ExternalId`s.
   */
  pack: (element: T) => PackedT | undefined
  unpack: (packed: PackedT) => T | undefined
}
export type SelectionOptions<T, PackedT> =
  | BaseSelectionOptions<T>
  | (BaseSelectionOptions<T> & SelectionPackingOptions<T, PackedT>)

export function useSelection<T>(
  navigator: NavigatorComposable,
  elementRects: Map<T, Rect>,
  options?: BaseSelectionOptions<T>,
): UseSelection<T, T>
export function useSelection<T, PackedT>(
  navigator: NavigatorComposable,
  elementRects: Map<T, Rect>,
  options: BaseSelectionOptions<T> & SelectionPackingOptions<T, PackedT>,
): UseSelection<T, PackedT>
/** TODO: Add docs */
export function useSelection<T, PackedT>(
  navigator: NavigatorComposable,
  elementRects: Map<T, Rect>,
  options: SelectionOptions<T, PackedT> = {},
): UseSelection<T, PackedT> {
  const BASE_DEFAULTS: Required<BaseSelectionOptions<T>> = {
    margin: 0,
    isValid: () => true,
    onSelected: () => {},
    onDeselected: () => {},
  }
  const PACKING_DEFAULTS: SelectionPackingOptions<T, T> = {
    pack: (element: T) => element,
    unpack: (packed: T) => packed,
  }
  return useSelectionImpl(
    navigator,
    elementRects,
    { ...BASE_DEFAULTS, ...options },
    'pack' in options ? options
      // The function signature ensures that if a `pack` function is not provided, PackedT = T.
    : (PACKING_DEFAULTS as unknown as SelectionPackingOptions<T, PackedT>),
  )
}

type UseSelection<T, PackedT> = ReturnType<typeof useSelectionImpl<T, PackedT>>
function useSelectionImpl<T, PackedT>(
  navigator: NavigatorComposable,
  elementRects: Map<T, Rect>,
  { margin, isValid, onSelected, onDeselected }: Required<BaseSelectionOptions<T>>,
  { pack, unpack }: SelectionPackingOptions<T, PackedT>,
) {
  const anchor = shallowRef<Vec2>()
  const focus = shallowRef<Vec2>()
  let initiallySelected = new Set<T>()
  // Selection, including elements that do not (currently) pass `isValid`.
  const rawSelected = shallowReactive(new Set<PackedT>())

  const unpackedRawSelected = computed(() => set.from(filterDefined(map(rawSelected, unpack))))
  const selected = computed(() => set.from(filter(unpackedRawSelected.value, isValid)))
  const isChanging = computed(() => anchor.value != null)
  const committedSelection = computed(() =>
    isChanging.value ? set.from(filter(initiallySelected, isValid)) : selected.value,
  )

  function readInitiallySelected() {
    initiallySelected = unpackedRawSelected.value
  }

  function setSelection(newSelection: Set<T>) {
    for (const id of newSelection) {
      const packed = pack(id)
      if (packed != null && !rawSelected.has(packed)) {
        rawSelected.add(packed)
        onSelected(id)
      }
    }

    for (const packed of rawSelected) {
      const id = unpack(packed)
      if (id == null || !newSelection.has(id)) {
        rawSelected.delete(packed)
        if (id != null) onDeselected(id)
      }
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

  /** Returns the single selected component, or an error. */
  function tryGetSoleSelection(): Result<T, string> {
    if (selected.value.size === 0) {
      return Err('No component selected')
    } else if (selected.value.size > 1) {
      return Err('Multiple components selected')
    } else {
      return Ok(set.first(selected.value)!)
    }
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

  const dragging = ref(false)

  const intersectingElements = computed<Set<T>>(() => {
    if (!dragging.value || anchor.value == null || focus.value == null) {
      return new Set()
    }
    const navigatorSpaceMargin = margin / navigator.scale

    const a = focus.value
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

  navigator.addHoldDragListener((state) => {
    dragging.value = state.dragging

    const eventPosition = navigator.clientToScenePos(Vec2.FromTuple(state.xy))

    if (state.first) {
      readInitiallySelected()
    }
    if (state.last) {
      if (anchor.value == null) {
        // If there was no drag, we want to handle "clicking-off" selected nodes.
        selectionEventHandler(state.event)
      }
      anchor.value = undefined
      focus.value = undefined
      initiallySelected.clear()
    } else if (state.dragging) {
      if (anchor.value == null) {
        anchor.value = eventPosition
      }
      focus.value = eventPosition
      selectionEventHandler(state.event)
    }
  })

  return {
    // === Selected nodes ===
    selected,
    selectAll: () => {
      for (const id of elementRects.keys()) {
        const packed = pack(id)
        if (packed) rawSelected.add(packed)
      }
    },
    deselectAll: () => rawSelected.clear(),
    isSelected: (element: T) => {
      const packed = pack(element)
      return packed != null && rawSelected.has(packed)
    },
    committedSelection,
    setSelection,
    tryGetSoleSelection,
    // === Selection changes ===
    anchor,
    focus,
    isChanging,
    // === Mouse events ===
    handleSelectionOf,
  }
}

// === Hover tracking for nodes and ports ===

/** TODO: Add docs */
export function useGraphHover(isPortEnabled: (port: PortId) => boolean) {
  const hoveredElement = shallowRef<Element>()

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
    return dataAttribute<NodeId>(element, 'nodeId')
  })

  return { hoveredNode, hoveredPort }
}
