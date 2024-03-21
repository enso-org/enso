/** @file A class for handling navigation between elements on a 2D plane. */
import * as object from '#/utilities/object'

// =================
// === Direction ===
// =================

/** The four cardinal directions. */
export enum Direction {
  left = 'left',
  right = 'right',
  up = 'up',
  down = 'down',
}

/** Return an object that is a mapping from every {@link Direction} to a specific value,
 * using the given mapping function. */
function mapDirections<T>(map: (direction: Direction) => T): Readonly<Record<Direction, T>> {
  return {
    [Direction.left]: map(Direction.left),
    [Direction.right]: map(Direction.right),
    [Direction.up]: map(Direction.up),
    [Direction.down]: map(Direction.down),
  }
}

// ===================
// === Navigator2D ===
// ===================

/** The neighbors of an element in each of the four cardinal directions. */
type ElementNeighbors = Readonly<Record<Direction, Element | null>>

/** All data associated with an element. */
interface ElementData extends Omit<Required<Navigator2DElementOptions>, 'focusPrimaryChild'> {
  readonly boundingBox: DOMRectReadOnly
  readonly neighbors: ElementNeighbors
  readonly allowedDirections: Readonly<Record<Direction, boolean>>
  readonly focusWhenPressed: Readonly<Record<Direction, (() => void) | null>>
  readonly dispose: () => void
}

/** Options when registering a element with a {@link Navigator2D}. */
interface Navigator2DElementOptions {
  /** The child that should be focused instead of the parent (if any).
   * Used as the fallback . */
  readonly focusPrimaryChild?: () => void
  /** The child that should be focused instead of the parent (if any),
   * when entering this element from the given direction. */
  readonly focusWhenPressed?: Partial<Record<Direction, (() => void) | null>>
  readonly allowedDirections?: Partial<Record<Direction, boolean>>
}

/** Options for a {@link Navigator2D}. */
interface Navigator2DOptions {
  readonly directionKeys?: Navigator2D['directionKeys']
}

/** Handle navigation between elements on a 2D plane. */
export default class Navigator2D {
  directionKeys: Record<Direction, string> = {
    [Direction.left]: 'ArrowLeft',
    [Direction.right]: 'ArrowRight',
    [Direction.up]: 'ArrowUp',
    [Direction.down]: 'ArrowDown',
  }
  private isLayoutDirty = true
  private readonly focusedElements = new Set<Element>()
  private readonly elements = new Map<Element, ElementData>()
  private readonly resizeObserver = new ResizeObserver(entries => {
    for (const entry of entries) {
      const data = this.elements.get(entry.target)
      if (data != null) {
        this.isLayoutDirty = true
        // This usage of `unsafeMutable` is SAFE, as `boundingBox` is mutable internally.
        object.unsafeMutable(data).boundingBox = entry.target.getBoundingClientRect()
      }
    }
  })

  /** Create a {@link Navigator2D}. */
  constructor(options: Navigator2DOptions = {}) {
    this.directionKeys = options.directionKeys ?? this.directionKeys
  }

  /** Recomputes the neighors of all elements.
   *
   * Full layout recomputations are expensive, but should amortize the cost of sorting the arrays. */
  recomputeLayout() {
    this.isLayoutDirty = false
    const datas = Array.from(this.elements.entries(), entry => {
      const [element, data] = entry
      const x = data.boundingBox.left + data.boundingBox.width / 2
      const y = data.boundingBox.top + data.boundingBox.height / 2
      return { element, data, x, y }
      // It is fine to not update neighbors of elements that are not visible.
    }).filter(data => data.data.boundingBox.width > 0 || data.data.boundingBox.height > 0)
    const byHorizontalCenter = [...datas].sort((a, b) => a.x - b.x)
    for (const data of byHorizontalCenter) {
      let nearestLeftNeighbor: Element | null = null
      let leftNeighborDistance = Infinity
      let nearestRightNeighbor: Element | null = null
      let rightNeighborDistance = Infinity
      for (const otherData of datas) {
        const distanceFromLeft = data.data.boundingBox.left - otherData.data.boundingBox.right
        const distanceFromRight = otherData.data.boundingBox.left - data.data.boundingBox.right
        const horizontalDistance = Math.max(distanceFromLeft, distanceFromRight)
        // The horizontal spans MUST NOT overlap.
        if (data.element !== otherData.element && horizontalDistance >= 0) {
          const distance = horizontalDistance + Math.abs(data.y - otherData.y)
          if (otherData.x < data.x) {
            if (distance < leftNeighborDistance) {
              nearestLeftNeighbor = otherData.element
              leftNeighborDistance = distance
            }
          } else if (otherData.x > data.x) {
            if (distance < rightNeighborDistance) {
              nearestRightNeighbor = otherData.element
              rightNeighborDistance = distance
            }
          }
        }
      }
      // This usage of `unsafeMutable` is SAFE, as `neighbors` is mutable internally.
      const neighbors = object.unsafeMutable(data.data.neighbors)
      neighbors[Direction.left] = nearestLeftNeighbor
      neighbors[Direction.right] = nearestRightNeighbor
    }
    const byVerticalCenter = [...datas].sort((a, b) => a.y - b.y)
    for (const data of byVerticalCenter) {
      let nearestAboveNeighbor: Element | null = null
      let aboveNeighborDistance = Infinity
      let nearestBelowNeighbor: Element | null = null
      let belowNeighborDistance = Infinity
      for (const otherData of datas) {
        const distanceFromTop = data.data.boundingBox.top - otherData.data.boundingBox.bottom
        const distanceFromBottom = otherData.data.boundingBox.top - data.data.boundingBox.bottom
        const verticalDistance = Math.max(distanceFromTop, distanceFromBottom)
        // The vertical spans MUST NOT overlap.
        if (data.element !== otherData.element && verticalDistance >= 0) {
          const distance = Math.abs(data.x - otherData.x) + verticalDistance
          if (otherData.y < data.y) {
            if (distance < aboveNeighborDistance) {
              nearestAboveNeighbor = otherData.element
              aboveNeighborDistance = distance
            }
          } else if (otherData.y > data.y) {
            if (distance < belowNeighborDistance) {
              nearestBelowNeighbor = otherData.element
              belowNeighborDistance = distance
            }
          }
        }
      }
      // This usage of `unsafeMutable` is SAFE, as `neighbors` is mutable internally.
      const neighbors = object.unsafeMutable(data.data.neighbors)
      neighbors[Direction.up] = nearestAboveNeighbor
      neighbors[Direction.down] = nearestBelowNeighbor
    }
  }

  /** Keydown handler. Should only be declared once, globally.
   * MUST be bound to this `Navigator2D` first using `.bind(navigator)`. */
  onKeyDown(event: KeyboardEvent) {
    let nearestFocusedParent = event.target instanceof Element ? event.target : null
    while (nearestFocusedParent != null && !this.focusedElements.has(nearestFocusedParent)) {
      nearestFocusedParent = nearestFocusedParent.parentElement
    }
    const data = nearestFocusedParent == null ? null : this.elements.get(nearestFocusedParent)
    const direction =
      event.key === this.directionKeys[Direction.up]
        ? Direction.up
        : event.key === this.directionKeys[Direction.down]
          ? Direction.down
          : event.key === this.directionKeys[Direction.left]
            ? Direction.left
            : event.key === this.directionKeys[Direction.right]
              ? Direction.right
              : null
    if (data != null && direction != null) {
      if (this.isLayoutDirty) {
        this.recomputeLayout()
      }
      const targetNeighbor = data.neighbors[direction]
      const focusTargetNeighbor =
        targetNeighbor instanceof HTMLElement ? targetNeighbor.focus.bind(null) : null
      const focus =
        targetNeighbor == null
          ? null
          : this.elements.get(targetNeighbor)?.focusWhenPressed[direction] ?? focusTargetNeighbor
      if (focus != null) {
        event.preventDefault()
        event.stopImmediatePropagation()
        focus()
      }
    }
  }

  /** Watch for layout changes on an element.
   * It is preferred to register a scroll container instead of its child, when possible,
   * because the scroll container resizes less often. */
  register(element: Element, options: Navigator2DElementOptions = {}) {
    const onFocusIn = () => {
      this.focusedElements.add(element)
    }
    element.addEventListener('focusin', onFocusIn)
    const onFocusOut = () => {
      this.focusedElements.delete(element)
    }
    element.addEventListener('focusout', onFocusOut)
    this.resizeObserver.observe(element)
    const mutationObserver = new MutationObserver(entries => {
      for (const entry of entries) {
        if (entry.target instanceof Element) {
          const data = this.elements.get(entry.target)
          if (data != null) {
            const oldBoundingBox = data.boundingBox
            const newBoundingBox = entry.target.getBoundingClientRect()
            if (
              newBoundingBox.left !== oldBoundingBox.left ||
              newBoundingBox.top !== oldBoundingBox.top ||
              newBoundingBox.width !== oldBoundingBox.width ||
              newBoundingBox.height !== oldBoundingBox.height
            ) {
              this.isLayoutDirty = true
              // This usage of `unsafeMutable` is SAFE, as `boundingBox` is mutable internally.
              object.unsafeMutable(data).boundingBox = newBoundingBox
            }
          }
        }
      }
      this.isLayoutDirty = true
    })
    mutationObserver.observe(element, { attributes: true, attributeFilter: ['style', 'class'] })
    const dispose = () => {
      element.removeEventListener('focusin', onFocusIn)
      element.removeEventListener('focusout', onFocusOut)
      mutationObserver.disconnect()
      this.resizeObserver.unobserve(element)
      this.elements.delete(element)
    }
    const defaultAllowedDirections = options.allowedDirections == null
    this.elements.set(element, {
      boundingBox: new DOMRectReadOnly(),
      neighbors: mapDirections(() => null),
      allowedDirections: mapDirections(
        direction => options.allowedDirections?.[direction] ?? defaultAllowedDirections
      ),
      focusWhenPressed: mapDirections(direction =>
        // This line is specialcasing `null` but not `undefined`.
        // eslint-disable-next-line eqeqeq
        options.focusWhenPressed?.[direction] === null
          ? null
          : options.focusWhenPressed?.[direction] ?? options.focusPrimaryChild ?? null
      ),
      dispose,
    })
    return () => {
      this.unregister(element)
    }
  }

  /** Stop watching for layout changes on an element. */
  unregister(element: Element) {
    this.elements.get(element)?.dispose()
  }
}
