import {
  computed,
  onMounted,
  onUnmounted,
  proxyRefs,
  ref,
  type Ref,
  shallowRef,
  watch,
  watchEffect,
  type WatchSource,
} from 'vue'
import { Vec2 } from './vec2'

/**
 * Add an event listener on an {@link Element} for the duration of the component's lifetime.
 * @param target element on which to register the event
 * @param event name of event to register
 * @param handler event handler
 */
export function useElementEvent<K extends keyof ElementEventMap>(
  target: Element,
  event: K,
  handler: (e: ElementEventMap[K]) => void,
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
 * Add an event listener on window for the duration of component lifetime.
 * @param event name of event to register
 * @param handler event handler
 */
export function useWindowEvent<K extends keyof WindowEventMap>(
  event: K,
  handler: (e: WindowEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void {
  onMounted(() => {
    window.addEventListener(event, handler, options)
  })
  onUnmounted(() => {
    window.removeEventListener(event, handler, options)
  })
}

/**
 * Add an event listener on document for the duration of component lifetime.
 * @param event name of event to register
 * @param handler event handler
 */
export function useDocumentEvent<K extends keyof DocumentEventMap>(
  event: K,
  handler: (e: DocumentEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void {
  onMounted(() => {
    document.addEventListener(event, handler, options)
  })
  onUnmounted(() => {
    document.removeEventListener(event, handler, options)
  })
}

/**
 * Add an event listener on window for the duration of condition being true.
 * @param condition the condition that determines if event is bound
 * @param event name of event to register
 * @param handler event handler
 */
export function useWindowEventConditional<K extends keyof WindowEventMap>(
  event: K,
  condition: WatchSource<boolean>,
  handler: (e: WindowEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void {
  watch(condition, (conditionMet, _, onCleanup) => {
    if (conditionMet) {
      window.addEventListener(event, handler, options)
      onCleanup(() => window.removeEventListener(event, handler, options))
    }
  })
}

/**
 * Add an event listener on document for the duration of condition being true.
 * @param condition the condition that determines if event is bound
 * @param event name of event to register
 * @param handler event handler
 */
export function useDocumentEventConditional<K extends keyof DocumentEventMap>(
  event: K,
  condition: WatchSource<boolean>,
  handler: (e: DocumentEventMap[K]) => void,
  options?: boolean | AddEventListenerOptions,
): void {
  watch(condition, (conditionMet, _, onCleanup) => {
    if (conditionMet) {
      document.addEventListener(event, handler, options)
      onCleanup(() => document.removeEventListener(event, handler, options))
    }
  })
}

// const hasWindow = typeof window !== 'undefined'
// const platform = hasWindow ? window.navigator?.platform ?? '' : ''
// const isMacLike = /(Mac|iPhone|iPod|iPad)/i.test(platform)

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
): Ref<Vec2> {
  const sizeRef = shallowRef<Vec2>(Vec2.Zero())
  const observer = new ResizeObserver((entries) => {
    let rect: { width: number; height: number } | null = null
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
      sizeRef.value = new Vec2(rect.width, rect.height)
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
  initial: Vec2
  /** Absolute event position, equivalent to clientX/Y. */
  absolute: Vec2
  /** Event position relative to the initial position. Total movement of the drag so far. */
  relative: Vec2
  /** Difference of the event position since last event. */
  delta: Vec2
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
  let initialGrabPos: Vec2 | null = null
  let lastPos: Vec2 | null = null

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
      lastPos = new Vec2(e.clientX, e.clientY)
    }
  }

  useWindowEventConditional('pointerup', isTracking, (e: PointerEvent) => {
    if (trackedPointer.value === e.pointerId) {
      e.preventDefault()
      doStop(e)
    }
  })

  useWindowEventConditional('pointermove', isTracking, (e: PointerEvent) => {
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
        initialGrabPos = new Vec2(e.clientX, e.clientY)
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

function computePosition(event: PointerEvent, initial: Vec2, last: Vec2): EventPosition {
  return {
    initial,
    absolute: new Vec2(event.clientX, event.clientY),
    relative: new Vec2(event.clientX - initial.x, event.clientY - initial.y),
    delta: new Vec2(event.clientX - last.x, event.clientY - last.y),
  }
}
