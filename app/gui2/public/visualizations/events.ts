import {
  computed,
  onMounted,
  onUnmounted,
  proxyRefs,
  ref,
  shallowRef,
  watch,
  watchEffect,
  type Ref,
  type WatchSource,
} from 'vue'

/**
 * Add an event listener for the duration of the component's lifetime.
 * @param target element on which to register the event
 * @param event name of event to register
 * @param handler event handler
 */
export function useEvent<K extends keyof DocumentEventMap>(
  target: Document,
  event: K,
  handler: (e: DocumentEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void
export function useEvent<K extends keyof WindowEventMap>(
  target: Window,
  event: K,
  handler: (e: WindowEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void
export function useEvent<K extends keyof ElementEventMap>(
  target: Element,
  event: K,
  handler: (event: ElementEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void
export function useEvent(
  target: EventTarget,
  event: string,
  handler: (event: unknown) => void,
  options?: boolean | AddEventListenerOptions,
): void {
  onMounted(() => {
    target.addEventListener(event, handler, options)
  })
  onUnmounted(() => {
    target.removeEventListener(event, handler, options)
  })
}

/**
 * Add an event listener for the duration of condition being true.
 * @param target element on which to register the event
 * @param condition the condition that determines if event is bound
 * @param event name of event to register
 * @param handler event handler
 */
export function useEventConditional<K extends keyof DocumentEventMap>(
  target: Document,
  event: K,
  condition: WatchSource<boolean>,
  handler: (e: DocumentEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void
export function useEventConditional<K extends keyof WindowEventMap>(
  target: Window,
  event: K,
  condition: WatchSource<boolean>,
  handler: (e: WindowEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void
export function useEventConditional<K extends keyof ElementEventMap>(
  target: Element,
  event: K,
  condition: WatchSource<boolean>,
  handler: (event: ElementEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void
export function useEventConditional(
  target: EventTarget,
  event: string,
  condition: WatchSource<boolean>,
  handler: (event: unknown) => void,
  options?: boolean | AddEventListenerOptions,
): void
export function useEventConditional(
  target: EventTarget,
  event: string,
  condition: WatchSource<boolean>,
  handler: (event: unknown) => void,
  options?: boolean | AddEventListenerOptions,
): void {
  watch(condition, (conditionMet, _, onCleanup) => {
    if (conditionMet) {
      target.addEventListener(event, handler, options)
      onCleanup(() => target.removeEventListener(event, handler, options))
    }
  })
}

interface Position {
  x: number
  y: number
}

interface Size {
  width: number
  height: number
}

/**
 * Get DOM node size and keep it up to date.
 *
 * # Warning:
 * Updating DOM node layout based on values derived from their size can introduce unwanted feedback
 * loops across the script and layout reflow. Avoid doing that.
 *
 * @param elementRef DOM node to observe.
 * @returns Reactive value with the DOM node size.
 */
export function useResizeObserver(
  elementRef: Ref<Element | undefined | null>,
  useContentRect = true,
): Ref<Size> {
  const sizeRef = shallowRef<Size>({ width: 0, height: 0 })
  const observer = new ResizeObserver((entries) => {
    let rect: Size | null = null
    for (const entry of entries) {
      if (entry.target === elementRef.value) {
        if (useContentRect) {
          rect = entry.contentRect
        } else {
          rect = entry.target.getBoundingClientRect()
        }
      }
    }
    if (rect != null) {
      sizeRef.value = { width: rect.width, height: rect.height }
    }
  })

  watchEffect((onCleanup) => {
    const element = elementRef.value
    if (element != null) {
      observer.observe(element)
      onCleanup(() => {
        if (elementRef.value != null) {
          observer.unobserve(element)
        }
      })
    }
  })

  return sizeRef
}

export interface EventPosition {
  /** The event position at the initialization of the drag. */
  initial: Position
  /** Absolute event position, equivalent to clientX/Y. */
  absolute: Position
  /** Event position relative to the initial position. Total movement of the drag so far. */
  relative: Position
  /** Difference of the event position since last event. */
  delta: Position
}

type PointerEventType = 'start' | 'move' | 'stop'

/**
 * A mask of all available pointer buttons. The values are compatible with DOM's `PointerEvent.buttons` value. The mask values
 * can be ORed together to create a mask of multiple buttons.
 */
export const enum PointerButtonMask {
  /** No buttons are pressed. */
  Empty = 0,
  /** Main mouse button, usually left. */
  Main = 1,
  /** Secondary mouse button, usually right. */
  Secondary = 2,
  /** Auxiliary mouse button, usually middle or wheel press. */
  Auxiliary = 4,
  /** Additional fourth mouse button, usually assigned to "browser back" action. */
  ExtBack = 8,
  /** Additional fifth mouse button, usually assigned to "browser forward" action. */
  ExtForward = 16,
}

/**
 * Register for a pointer dragging events.
 *
 * @param handler callback on any pointer event
 * @param requiredButtonMask declare which buttons to look for. The value represents a `PointerEvent.buttons` mask.
 * @returns
 */
export function usePointer(
  handler: (pos: EventPosition, event: PointerEvent, eventType: PointerEventType) => void,
  requiredButtonMask: number = PointerButtonMask.Main,
) {
  const trackedPointer: Ref<number | null> = ref(null)
  let trackedElement: Element | null = null
  let initialGrabPos: Position | null = null
  let lastPos: Position | null = null

  const isTracking = () => trackedPointer.value != null

  function doStop(e: PointerEvent) {
    if (trackedElement != null && trackedPointer.value != null) {
      trackedElement.releasePointerCapture(trackedPointer.value)
    }

    trackedPointer.value = null

    if (trackedElement != null && initialGrabPos != null && lastPos != null) {
      handler(computePosition(e, initialGrabPos, lastPos), e, 'stop')
      lastPos = null
      trackedElement = null
    }
  }

  function doMove(e: PointerEvent) {
    if (trackedElement != null && initialGrabPos != null && lastPos != null) {
      handler(computePosition(e, initialGrabPos, lastPos), e, 'move')
      lastPos = { x: e.clientX, y: e.clientY }
    }
  }

  useEventConditional(window, 'pointerup', isTracking, (e: PointerEvent) => {
    if (trackedPointer.value === e.pointerId) {
      e.preventDefault()
      doStop(e)
    }
  })

  useEventConditional(window, 'pointermove', isTracking, (e: PointerEvent) => {
    if (trackedPointer.value === e.pointerId) {
      e.preventDefault()
      // handle release of all masked buttons as stop
      if ((e.buttons & requiredButtonMask) != 0) {
        doMove(e)
      } else {
        doStop(e)
      }
    }
  })

  const events = {
    pointerdown(e: PointerEvent) {
      // pointers should not respond to unmasked mouse buttons
      if ((e.buttons & requiredButtonMask) == 0) {
        return
      }

      if (trackedPointer.value == null && e.currentTarget instanceof Element) {
        e.preventDefault()
        trackedPointer.value = e.pointerId
        trackedElement = e.currentTarget
        trackedElement.setPointerCapture(e.pointerId)
        initialGrabPos = { x: e.clientX, y: e.clientY }
        lastPos = initialGrabPos
        handler(computePosition(e, initialGrabPos, lastPos), e, 'start')
      }
    },
  }

  return proxyRefs({
    events,
    dragging: computed(() => trackedPointer.value != null),
  })
}

function computePosition(event: PointerEvent, initial: Position, last: Position): EventPosition {
  return {
    initial,
    absolute: { x: event.clientX, y: event.clientY },
    relative: { x: event.clientX - initial.x, y: event.clientY - initial.y },
    delta: { x: event.clientX - last.x, y: event.clientY - last.y },
  }
}
