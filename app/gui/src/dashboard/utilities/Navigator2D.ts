/** @file A class for handling navigation between elements on a 2D plane. */
import type * as React from 'react'

import * as detect from 'enso-common/src/detect'

import * as eventModule from '#/utilities/event'
import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

/** A singleton function that returns true. */
function returnTrue() {
  return true
}

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
const DIRECTIONS = Object.values(Direction)

/**
 * Return an object that is a mapping from every {@link Direction} to a specific value,
 * using the given mapping function.
 */
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

/** Metadata containing an element and its distance. */
interface ElementAndDistance {
  readonly element: Element
  readonly distance: number
}

/** The neighbors of an element in each of the four cardinal directions. */
type ElementNeighbors = Readonly<Record<Direction, readonly Element[]>>

/** All data associated with an element. */
interface ElementData extends Omit<Required<Navigator2DElementOptions>, 'focusPrimaryChild'> {
  readonly boundingBox: DOMRectReadOnly
  readonly neighbors: ElementNeighbors
  readonly focusWhenPressed: Readonly<Record<Direction, (() => void) | null>>
  readonly dispose: () => void
}

/** Options when registering a element with a {@link Navigator2D}. */
interface Navigator2DElementOptions {
  /** A function that returns whether navigation is currently enabled. */
  readonly allowNavigation?: () => boolean
  /**
   * The child that should be focused instead of the parent (if any).
   * Used as the fallback .
   */
  readonly focusPrimaryChild?: () => void
  /**
   * The child that should be focused instead of the parent (if any),
   * when entering this element from the given direction.
   */
  readonly focusWhenPressed?: Partial<Record<Direction, (() => void) | null>>
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
  private readonly focusRoots: Element[] = []
  private readonly currentNeighbors = new Set<HTMLOrSVGElement>()
  private readonly focusedElements = new Set<Element>()
  private readonly elements = new Map<Element, ElementData>()
  private readonly resizeObserver = new ResizeObserver((entries) => {
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
    if (detect.IS_DEV_MODE) {
      let watching = false
      let lastFocusedElement: Element | null = null
      const onFocusIn = (event: FocusEvent) => {
        if (event.target !== lastFocusedElement && event.target instanceof Element) {
          lastFocusedElement = event.target
          const target = event.target
          // Wait for next tick, to ensure the current parent has been added to
          // `focusedElements`.
          setTimeout(() => {
            for (const neighbor of this.currentNeighbors) {
              delete neighbor.dataset.navigator2dNeighbor
            }
            this.currentNeighbors.clear()
            for (const direction of DIRECTIONS) {
              const neighbor = this.neighborInDirection(target, direction)
              if (neighbor instanceof HTMLElement || neighbor instanceof SVGElement) {
                this.currentNeighbors.add(neighbor)
                neighbor.dataset.navigator2dNeighbor = ''
              }
            }
          })
        }
      }
      const onFocusOut = (event: FocusEvent) => {
        if (event.target === lastFocusedElement) {
          lastFocusedElement = null
          for (const neighbor of this.currentNeighbors) {
            delete neighbor.dataset.navigator2dNeighbor
          }
          this.currentNeighbors.clear()
        }
      }
      const mutationObserver = new MutationObserver(() => {
        if ('debugNavigator2d' in document.body.dataset) {
          if (!watching) {
            watching = true
            // eslint-disable-next-line no-restricted-properties
            console.debug('Showing `Navigator2D` neighbors.')
            document.body.addEventListener('focusin', onFocusIn, { capture: true })
            document.body.addEventListener('focusout', onFocusOut, { capture: true })
          }
        } else {
          if (watching) {
            watching = false
            // eslint-disable-next-line no-restricted-properties
            console.debug('Stopped showing `Navigator2D` neighbors.')
            document.body.removeEventListener('focusin', onFocusIn, { capture: true })
            document.body.removeEventListener('focusout', onFocusOut, { capture: true })
          }
        }
      })
      mutationObserver.observe(document.body, {
        attributes: true,
        attributeFilter: ['data-debug-navigator2d'],
      })
    }
  }

  /** Return the current innermost focus root. */
  private get focusRoot() {
    return this.focusRoots[this.focusRoots.length - 1] ?? null
  }

  /** Push a new focus root as the innermost focus root. */
  pushFocusRoot(focusRoot: Element) {
    this.focusRoots.push(focusRoot)
    return () => {
      this.popFocusRoot(focusRoot)
    }
  }

  /**
   * Pop a focus root, *and all focus roots after it*, if it exists in the stack of focus roots.
   * Does nothing if it is not in the stack.
   */
  popFocusRoot(focusRoot: Element) {
    const index = this.focusRoots.indexOf(focusRoot)
    if (index !== -1) {
      this.focusRoots.splice(index)
    }
  }

  /**
   * Recomputes the neighbors of all elements.
   *
   * Full layout recomputations are expensive, but should amortize the cost of sorting the arrays.
   */
  recomputeLayout() {
    this.isLayoutDirty = false
    const datas = Array.from(this.elements.entries(), (entry) => {
      const [element, data] = entry
      const x = data.boundingBox.left + data.boundingBox.width / 2
      const y = data.boundingBox.top + data.boundingBox.height / 2
      return { element, data, x, y }
      // It is fine to not update neighbors of elements that are not visible.
    }).filter((data) => data.data.boundingBox.width > 0 || data.data.boundingBox.height > 0)
    const byHorizontalCenter = [...datas].sort((a, b) => a.x - b.x)
    for (const data of byHorizontalCenter) {
      const leftNeighbors: ElementAndDistance[] = []
      const rightNeighbors: ElementAndDistance[] = []
      for (const otherData of datas) {
        const distanceFromLeft = data.data.boundingBox.left - otherData.data.boundingBox.right
        const distanceFromRight = otherData.data.boundingBox.left - data.data.boundingBox.right
        const horizontalDistance = Math.max(distanceFromLeft, distanceFromRight)
        // The horizontal spans MUST NOT overlap.
        if (data.element !== otherData.element && horizontalDistance >= 0) {
          const distance = horizontalDistance + Math.abs(data.y - otherData.y)
          if (otherData.x < data.x) {
            leftNeighbors.push({ element: otherData.element, distance })
          } else if (otherData.x > data.x) {
            rightNeighbors.push({ element: otherData.element, distance })
          }
        }
      }
      // This usage of `unsafeMutable` is SAFE, as `neighbors` is mutable internally.
      const neighbors = object.unsafeMutable(data.data.neighbors)
      neighbors[Direction.left] = leftNeighbors
        .sort((a, b) => a.distance - b.distance)
        .map((metadata) => metadata.element)
      neighbors[Direction.right] = rightNeighbors
        .sort((a, b) => a.distance - b.distance)
        .map((metadata) => metadata.element)
    }
    const byVerticalCenter = [...datas].sort((a, b) => a.y - b.y)
    for (const data of byVerticalCenter) {
      const aboveNeighbors: ElementAndDistance[] = []
      const belowNeighbors: ElementAndDistance[] = []
      for (const otherData of datas) {
        const distanceFromTop = data.data.boundingBox.top - otherData.data.boundingBox.bottom
        const distanceFromBottom = otherData.data.boundingBox.top - data.data.boundingBox.bottom
        const verticalDistance = Math.max(distanceFromTop, distanceFromBottom)
        // The vertical spans MUST NOT overlap.
        if (data.element !== otherData.element && verticalDistance >= 0) {
          const distance = Math.abs(data.x - otherData.x) + verticalDistance
          if (otherData.y < data.y) {
            aboveNeighbors.push({ element: otherData.element, distance })
          } else if (otherData.y > data.y) {
            belowNeighbors.push({ element: otherData.element, distance })
          }
        }
      }
      // This usage of `unsafeMutable` is SAFE, as `neighbors` is mutable internally.
      const neighbors = object.unsafeMutable(data.data.neighbors)
      neighbors[Direction.up] = aboveNeighbors
        .sort((a, b) => a.distance - b.distance)
        .map((metadata) => metadata.element)
      neighbors[Direction.down] = belowNeighbors
        .sort((a, b) => a.distance - b.distance)
        .map((metadata) => metadata.element)
    }
  }

  /**
   * Keydown handler. Should only be declared once per focus root (including the global one on
   * `document`). MUST be bound to this `Navigator2D` first using `.bind(navigator)`.
   */
  onKeyDown(event: KeyboardEvent | React.KeyboardEvent) {
    if (event.defaultPrevented) {
      return
    }
    let nearestFocusedParent = event.target instanceof Element ? event.target : null
    while (nearestFocusedParent != null && !this.focusedElements.has(nearestFocusedParent)) {
      nearestFocusedParent = nearestFocusedParent.parentElement
    }
    const data = nearestFocusedParent == null ? null : this.elements.get(nearestFocusedParent)
    const direction =
      event.key === this.directionKeys[Direction.up] ? Direction.up
      : event.key === this.directionKeys[Direction.down] ? Direction.down
      : event.key === this.directionKeys[Direction.left] ? Direction.left
      : event.key === this.directionKeys[Direction.right] ? Direction.right
      : null
    const shouldHandleEvent =
      data?.allowNavigation() === true && direction != null && event.target instanceof Element
    let shouldHandleKey = true
    const isArrowKeyEvent = eventModule.isArrowKeyEvent(event)
    if (shouldHandleEvent && isArrowKeyEvent && eventModule.isElementTextInput(event.target)) {
      if (eventModule.isElementSingleLineTextInput(event.target)) {
        const selectionIndex =
          event.target.selectionStart === event.target.selectionEnd ?
            event.target.selectionStart
          : null
        shouldHandleKey =
          (selectionIndex === 0 || event.key !== 'ArrowLeft') &&
          (selectionIndex === event.target.value.length || event.key !== 'ArrowRight')
      } else {
        const selectionIndex =
          (
            event.target instanceof HTMLTextAreaElement &&
            event.target.selectionStart === event.target.selectionEnd
          ) ?
            event.target.selectionStart
          : null
        const length =
          event.target instanceof HTMLTextAreaElement ? event.target.value.length : null
        shouldHandleKey =
          (selectionIndex === 0 || event.key !== 'ArrowLeft') &&
          (selectionIndex === length || event.key !== 'ArrowRight') &&
          event.key !== 'ArrowUp' &&
          event.key !== 'ArrowDown'
      }
    }
    if (shouldHandleEvent && shouldHandleKey) {
      const neighbor = this.neighborInDirection(event.target, direction)
      const focusTargetNeighbor = neighbor instanceof HTMLElement ? neighbor.focus.bind(null) : null
      const focus =
        neighbor == null ? null : (
          this.elements.get(neighbor)?.focusWhenPressed[direction] ?? focusTargetNeighbor
        )
      if (focus != null) {
        event.preventDefault()
        if ('stopImmediatePropagation' in event) {
          event.stopImmediatePropagation()
        } else {
          event.stopPropagation()
        }
        focus()
      }
    }
  }

  /**
   * Watch for layout changes on an element.
   * It is preferred to register a scroll container instead of its child, when possible,
   * because the scroll container resizes less often.
   */
  register(element: Element, options: Navigator2DElementOptions = {}) {
    if (element.contains(document.activeElement)) {
      this.focusedElements.add(element)
    }
    const onFocusIn = () => {
      this.focusedElements.add(element)
    }
    element.addEventListener('focusin', onFocusIn, { capture: true })
    const onFocusOut = () => {
      this.focusedElements.delete(element)
    }
    element.addEventListener('focusout', onFocusOut, { capture: true })
    this.resizeObserver.observe(element)
    const mutationObserver = new MutationObserver((entries) => {
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
    this.elements.set(element, {
      boundingBox: element.getBoundingClientRect(),
      allowNavigation: options.allowNavigation ?? returnTrue,
      neighbors: mapDirections(() => []),
      focusWhenPressed: mapDirections((direction) =>
        // This line is specialcasing `null` but not `undefined`.
        // eslint-disable-next-line eqeqeq
        options.focusWhenPressed?.[direction] === null ?
          null
        : options.focusWhenPressed?.[direction] ?? options.focusPrimaryChild ?? null,
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

  /**
   * Keydown handler. Should only be declared once, globally.
   * MUST be bound to this `Navigator2D` first using `.bind(navigator)`.
   */
  private neighborInDirection(element: Element, direction: Direction) {
    let nearestFocusedParent: Element | null = element
    while (nearestFocusedParent != null && !this.focusedElements.has(nearestFocusedParent)) {
      nearestFocusedParent = nearestFocusedParent.parentElement
    }
    const data = nearestFocusedParent == null ? null : this.elements.get(nearestFocusedParent)
    if (data != null) {
      if (this.isLayoutDirty) {
        this.recomputeLayout()
      }
      const focusRoot = this.focusRoot
      const isNavigatingVertically = direction === Direction.up || direction === Direction.down
      const boundingBox = element.getBoundingClientRect()
      const targetNeighbors = data.neighbors[direction]
      let targetNeighbor: Element | null = null
      let minimumVerticalDistance = Infinity
      let minimumHorizontalDistance = Infinity
      for (const neighbor of targetNeighbors) {
        if (focusRoot != null && !focusRoot.contains(neighbor)) {
          continue
        }
        const neighborBoundingBox = neighbor.getBoundingClientRect()
        const distanceFromLeft = boundingBox.left - neighborBoundingBox.right
        const distanceFromRight = neighborBoundingBox.left - boundingBox.right
        const horizontalDistance = Math.max(0, distanceFromLeft, distanceFromRight)
        const distanceFromTop = boundingBox.top - neighborBoundingBox.bottom
        const distanceFromBottom = neighborBoundingBox.top - boundingBox.bottom
        const verticalDistance = Math.max(0, distanceFromTop, distanceFromBottom)
        if (isNavigatingVertically) {
          if (
            horizontalDistance < minimumHorizontalDistance ||
            (horizontalDistance === minimumHorizontalDistance &&
              verticalDistance < minimumVerticalDistance)
          ) {
            targetNeighbor = neighbor
            minimumHorizontalDistance = horizontalDistance
            minimumVerticalDistance = verticalDistance
          }
        } else {
          if (
            verticalDistance < minimumVerticalDistance ||
            (verticalDistance === minimumVerticalDistance &&
              horizontalDistance < minimumHorizontalDistance)
          ) {
            targetNeighbor = neighbor
            minimumHorizontalDistance = horizontalDistance
            minimumVerticalDistance = verticalDistance
          }
        }
      }
      return targetNeighbor ?? null
    } else {
      return null
    }
  }
}
