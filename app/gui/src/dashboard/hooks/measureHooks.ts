/**
 * @file
 *
 * This file contains the useMeasure hook, which is used to measure the size and position of an element.
 */
import { frame, useMotionValue } from 'framer-motion'

import { useEffect, useRef, useState } from 'react'
import { unsafeMutable } from '../utilities/object'
import { findScrollContainers, type HTMLOrSVGElement } from '../utilities/scrollContainers'
import { useDebouncedCallback } from './debounceCallbackHooks'
import { useEventCallback } from './eventCallbackHooks'
import { useEventListener } from './eventListenerHooks'
import { useUnmount } from './unmountHooks'

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
 * A type that represents the result of the useMeasure hook.
 */
type Result = [(element: HTMLOrSVGElement | null) => void, RectReadOnly | null, () => void]

/**
 * A type that represents the state of the useMeasure hook.
 */
interface State {
  readonly element: HTMLOrSVGElement | null
  readonly scrollContainers: HTMLOrSVGElement[] | null
  readonly lastBounds: RectReadOnly | null
}

/**
 * A type that represents a callback that is called when the element is resized.
 */
export type OnResizeCallback = (bounds: RectReadOnly) => void

/**
 * A type that represents the options for the useMeasure hook.
 */
export interface Options {
  readonly debounce?:
    | number
    | false
    | { readonly scroll: number | false; readonly resize: number | false }
  readonly scroll?: boolean
  readonly offsetSize?: boolean
  readonly onResize?: OnResizeCallback
  readonly onInitialMeasure?: OnResizeCallback
  readonly maxWait?: number | { readonly scroll: number; readonly resize: number }
  /**
   * Whether to use RAF to measure the element.
   */
  readonly useRAF?: boolean
  readonly isDisabled?: boolean
}

/**
 * Custom hook to measure the size and position of an element
 */
export function useMeasure(options: Options = {}): Result {
  const { onResize, onInitialMeasure } = options

  const [bounds, set] = useState<RectReadOnly | null>(null)

  const onResizeStableCallback = useEventCallback<OnResizeCallback>((nextBounds) => {
    set(nextBounds)

    onResize?.(nextBounds)
  })

  const onInitialMeasureStableCallback = useEventCallback<OnResizeCallback>((nextBounds) => {
    set(nextBounds)

    onInitialMeasure?.(nextBounds)
  })

  const [ref, forceRefresh] = useMeasureCallback({
    ...options,
    onResize: onResizeStableCallback,
    onInitialMeasure: onInitialMeasureStableCallback,
  })

  return [ref, bounds, forceRefresh] as const
}

/**
 * Helper hook that uses motion primitive to optimize renders, works best with motion components
 */
export function useMeasureSignal(options: Options = {}) {
  const { onResize, onInitialMeasure } = options

  const bounds = useMotionValue<RectReadOnly | null>(null)

  const onResizeStableCallback = useEventCallback<OnResizeCallback>((nextBounds) => {
    bounds.set(nextBounds)

    onResize?.(nextBounds)
  })

  const onInitialMeasureStableCallback = useEventCallback<OnResizeCallback>((nextBounds) => {
    bounds.set(nextBounds)

    onInitialMeasure?.(nextBounds)
  })

  const [ref, forceRefresh] = useMeasureCallback({
    ...options,
    onResize: onResizeStableCallback,
    onInitialMeasure: onInitialMeasureStableCallback,
  })

  return [ref, bounds, forceRefresh] as const
}

const DEFAULT_MAX_WAIT = 500

/**
 * Same as useMeasure, but doesn't rerender the component when the element is resized.
 * Instead, it calls the `onResize` callback with the new bounds. This is useful when you want to
 * measure the size of an element without causing a rerender.
 */
export function useMeasureCallback(options: Options & Required<Pick<Options, 'onResize'>>) {
  const {
    debounce = false,
    scroll = false,
    offsetSize = false,
    onResize,
    maxWait = DEFAULT_MAX_WAIT,
    useRAF = true,
    isDisabled = false,
    onInitialMeasure,
  } = options

  // keep all state in a ref
  const state = useRef<State>({
    element: null,
    scrollContainers: null,
    lastBounds: null,
  })
  // make sure to update state only as long as the component is truly mounted
  const mounted = useRef(false)

  const onResizeStableCallback = useEventCallback<OnResizeCallback>(onResize)

  const scrollMaxWait = typeof maxWait === 'number' ? maxWait : maxWait.scroll
  const resizeMaxWait = typeof maxWait === 'number' ? maxWait : maxWait.resize

  // set actual debounce values early, so effects know if they should react accordingly
  const scrollDebounce =
    typeof debounce === 'number' || debounce === false ? debounce : debounce.scroll
  const resizeDebounce =
    typeof debounce === 'number' || debounce === false ? debounce : debounce.resize

  const callback = useEventCallback(() => {
    frame.read(measureCallback)
  })

  const measureCallback = useEventCallback(() => {
    const element = state.current.element

    if (!element) return

    const { left, top, width, height, bottom, right, x, y } = element.getBoundingClientRect()

    const size = { left, top, width, height, bottom, right, x, y }

    if (element instanceof HTMLElement && offsetSize) {
      size.height = element.offsetHeight
      size.width = element.offsetWidth
    }

    if (mounted.current && !areBoundsEqual(state.current.lastBounds, size)) {
      if (state.current.lastBounds == null) {
        onInitialMeasure?.(size)
      }

      if (!isDisabled) {
        onResizeStableCallback(size)
      }

      unsafeMutable(state.current).lastBounds = size
    }
  })

  const resizeDebounceCallback = useDebouncedCallback(
    measureCallback,
    resizeDebounce,
    resizeMaxWait,
  )

  const scrollDebounceCallback = useDebouncedCallback(
    measureCallback,
    scrollDebounce,
    scrollMaxWait,
  )

  const [resizeObserver] = useState(() => new ResizeObserver(measureCallback))
  const [mutationObserver] = useState(() => new MutationObserver(measureCallback))

  const forceRefresh = useDebouncedCallback(callback, 0)

  // cleanup current scroll-listeners / observers
  const removeListeners = useEventCallback(() => {
    if (state.current.scrollContainers) {
      state.current.scrollContainers.forEach((element) => {
        element.removeEventListener('scroll', scrollDebounceCallback, true)
      })
      unsafeMutable(state.current).scrollContainers = null
    }

    resizeObserver.disconnect()
    mutationObserver.disconnect()
  })

  const addListeners = useEventCallback(() => {
    if (!state.current.element) return

    resizeObserver.observe(state.current.element)
    mutationObserver.observe(state.current.element, {
      attributes: true,
      attributeFilter: ['style', 'class'],
    })

    if (useRAF) {
      frame.read(() => {
        measureCallback()
      }, true)
    }

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
  const ref = useEventCallback((node: HTMLOrSVGElement | null) => {
    mounted.current = node != null

    if (!node || node === state.current.element) return

    removeListeners()

    unsafeMutable(state.current).element = node
    unsafeMutable(state.current).scrollContainers = findScrollContainers(node)

    measureCallback()

    addListeners()
  })

  // add general event listeners
  useEventListener('scroll', scrollDebounceCallback, window, {
    passive: true,
    capture: true,
    isDisabled: !scroll,
  })
  useEventListener('resize', resizeDebounceCallback, window, { passive: true })

  // respond to changes that are relevant for the listeners
  // respond to changes that are relevant for the listeners
  useEffect(() => {
    removeListeners()
    addListeners()
  }, [useRAF, scroll, removeListeners, addListeners])

  useUnmount(removeListeners)

  return [ref, forceRefresh] as const
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
function areBoundsEqual(a: RectReadOnly | null, b: RectReadOnly | null): boolean {
  if (a == null || b == null) return false

  return RECT_KEYS.every((key) => a[key] === b[key])
}
