/**
 * @file
 *
 * This file contains the useMeasure hook, which is used to measure the size and position of an element.
 */
import { useEffect, useRef, useState } from 'react'
import { unsafeMutable } from '../utilities/object'
import { useDebouncedCallback } from './debounceCallbackHooks'
import { useEventCallback } from './eventCallbackHooks'

/**
 * A read-only version of the DOMRect object.
 */
export interface RectReadOnly {
  readonly x: number
  readonly y: number
  readonly width: number
  readonly height: number
  readonly top: number
  readonly right: number
  readonly bottom: number
  readonly left: number
}

/**
 * A type that represents an HTML or SVG element.
 */
type HTMLOrSVGElement = HTMLElement | SVGElement

/**
 * A type that represents the result of the useMeasure hook.
 */
type Result = [(element: HTMLOrSVGElement | null) => void, RectReadOnly, () => void]

/**
 * A type that represents the state of the useMeasure hook.
 */
interface State {
  readonly element: HTMLOrSVGElement | null
  readonly scrollContainers: HTMLOrSVGElement[] | null
  readonly resizeObserver: ResizeObserver | null
  readonly lastBounds: RectReadOnly
}

/**
 * A type that represents the options for the useMeasure hook.
 */
export interface Options {
  readonly debounce?: number | { readonly scroll: number; readonly resize: number }
  readonly scroll?: boolean
  readonly polyfill?: { new (cb: ResizeObserverCallback): ResizeObserver }
  readonly offsetSize?: boolean
}

/**
 * Custom hook to measure the size and position of an element
 */
export function useMeasure(options: Options): Result {
  const { debounce = 0, scroll = false, offsetSize = false } = options

  const [bounds, set] = useState<RectReadOnly>(() => ({
    left: 0,
    top: 0,
    width: 0,
    height: 0,
    bottom: 0,
    right: 0,
    x: 0,
    y: 0,
  }))

  // keep all state in a ref
  const state = useRef<State>({
    element: null,
    scrollContainers: null,
    resizeObserver: null,
    lastBounds: bounds,
  })

  // set actual debounce values early, so effects know if they should react accordingly
  const scrollDebounce = typeof debounce === 'number' ? debounce : debounce.scroll
  const resizeDebounce = typeof debounce === 'number' ? debounce : debounce.resize

  // make sure to update state only as long as the component is truly mounted
  const mounted = useRef(false)
  useEffect(() => {
    mounted.current = true
    return () => void (mounted.current = false)
  })

  const callback = useEventCallback(() => {
    if (!state.current.element) return
    const { left, top, width, height, bottom, right, x, y } =
      state.current.element.getBoundingClientRect()

    const size = {
      left,
      top,
      width,
      height,
      bottom,
      right,
      x,
      y,
    }

    if (state.current.element instanceof HTMLElement && offsetSize) {
      size.height = state.current.element.offsetHeight
      size.width = state.current.element.offsetWidth
    }

    if (mounted.current && !areBoundsEqual(state.current.lastBounds, size)) {
      set((unsafeMutable(state.current).lastBounds = size))
    }
  })

  const resizeDebounceCallback = useDebouncedCallback(callback, [], resizeDebounce)
  const scrollDebounceCallback = useDebouncedCallback(callback, [], scrollDebounce)
  const forceRefresh = useDebouncedCallback(callback, [], 0)

  // cleanup current scroll-listeners / observers
  const removeListeners = useEventCallback(() => {
    if (state.current.scrollContainers) {
      state.current.scrollContainers.forEach((element) => {
        element.removeEventListener('scroll', scrollDebounceCallback, true)
      })
      unsafeMutable(state.current).scrollContainers = null
    }

    if (state.current.resizeObserver) {
      state.current.resizeObserver.disconnect()
      unsafeMutable(state.current).resizeObserver = null
    }
  })

  const addListeners = useEventCallback(() => {
    if (!state.current.element) return
    unsafeMutable(state.current).resizeObserver = new ResizeObserver(scrollDebounceCallback)
    state.current.resizeObserver?.observe(state.current.element)
    if (scroll && state.current.scrollContainers) {
      state.current.scrollContainers.forEach((scrollContainer) => {
        scrollContainer.addEventListener('scroll', scrollDebounceCallback, {
          capture: true,
          passive: true,
        })
      })
    }
  })

  // the ref we expose to the user
  const ref = (node: HTMLOrSVGElement | null) => {
    if (!node || node === state.current.element) return
    removeListeners()

    unsafeMutable(state.current).element = node
    unsafeMutable(state.current).scrollContainers = findScrollContainers(node)
    addListeners()
  }

  // add general event listeners
  useOnWindowScroll(scrollDebounceCallback, Boolean(scroll))
  useOnWindowResize(resizeDebounceCallback)

  // respond to changes that are relevant for the listeners
  useEffect(() => {
    removeListeners()
    addListeners()
  }, [scroll, scrollDebounceCallback, resizeDebounceCallback, removeListeners, addListeners])

  // remove all listeners when the components unmounts
  useEffect(() => removeListeners, [removeListeners])
  return [ref, bounds, forceRefresh]
}

/**
 * Adds a window resize event listener
 */
function useOnWindowResize(onWindowResize: (event: Event) => void) {
  useEffect(() => {
    const cb = onWindowResize
    window.addEventListener('resize', cb)
    return () => {
      window.removeEventListener('resize', cb)
    }
  }, [onWindowResize])
}

/**
 * Adds a window scroll event listener
 */
function useOnWindowScroll(onScroll: () => void, enabled: boolean) {
  useEffect(() => {
    if (enabled) {
      const cb = onScroll
      window.addEventListener('scroll', cb, { capture: true, passive: true })
      return () => {
        window.removeEventListener('scroll', cb, true)
      }
    }
  }, [onScroll, enabled])
}

// Returns a list of scroll offsets
/**
 * Finds all scroll containers that have overflow set to 'auto' or 'scroll'
 * @param element - The element to start searching from
 * @returns An array of scroll containers
 */
function findScrollContainers(element: HTMLOrSVGElement | null): HTMLOrSVGElement[] {
  const result: HTMLOrSVGElement[] = []
  if (!element || element === document.body) return result

  const { overflow, overflowX, overflowY } = window.getComputedStyle(element)
  if ([overflow, overflowX, overflowY].some((prop) => prop === 'auto' || prop === 'scroll')) {
    result.push(element)
  }
  return [...result, ...findScrollContainers(element.parentElement)]
}

// Checks if element boundaries are equal
const RECT_KEYS: readonly (keyof RectReadOnly)[] = [
  'x',
  'y',
  'top',
  'bottom',
  'left',
  'right',
  'width',
  'height',
]

/**
 * Compares two RectReadOnly objects to check if their boundaries are equal
 * @param a - First RectReadOnly object
 * @param b - Second RectReadOnly object
 * @returns boolean indicating whether the boundaries are equal
 */
function areBoundsEqual(a: RectReadOnly, b: RectReadOnly): boolean {
  return RECT_KEYS.every((key) => a[key] === b[key])
}
