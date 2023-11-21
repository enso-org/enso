import type { Opt } from '@/util/opt'
import { Vec2 } from '@/util/vec2'
import {
  computed,
  onScopeDispose,
  proxyRefs,
  ref,
  shallowRef,
  watch,
  watchEffect,
  type Ref,
  type WatchSource,
} from 'vue'

export function isClick(e: MouseEvent | PointerEvent) {
  if (e instanceof PointerEvent) return e.pointerId !== -1
  else return e.buttons !== 0
}

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
  target.addEventListener(event, handler, options)
  onScopeDispose(() => {
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

/** Whether any element currently has keyboard focus. */
export function keyboardBusy() {
  return document.activeElement != document.body
}

/** Whether focused element is within given element's subtree. */
export function focusIsIn(el: Element) {
  return el.contains(document.activeElement)
}

/**
 * Whether any element currently has keyboard focus, except for elements within given subtree.
 * When `el` is `null` or `undefined`, the function behaves as `keyboardBusy()`.
 */
export function keyboardBusyExceptIn(el: Opt<Element>) {
  return keyboardBusy() && (el == null || !focusIsIn(el))
}

const hasWindow = typeof window !== 'undefined'
const platform = hasWindow ? window.navigator?.platform ?? '' : ''
export const isMacLike = /(Mac|iPhone|iPod|iPad)/i.test(platform)

export function modKey(e: KeyboardEvent): boolean {
  return isMacLike ? e.metaKey : e.ctrlKey
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
): Ref<Vec2> {
  const sizeRef = shallowRef<Vec2>(Vec2.Zero)
  if (typeof ResizeObserver === 'undefined') {
    // Fallback implementation for browsers/test environment that do not support ResizeObserver:
    // Grab the size of the element every time the ref is assigned, or when the page is resized.
    function refreshSize() {
      const element = elementRef.value
      if (element != null) {
        const rect = element.getBoundingClientRect()
        sizeRef.value = new Vec2(rect.width, rect.height)
      }
    }
    watchEffect(refreshSize)
    useEvent(window, 'resize', refreshSize)
    return sizeRef
  }
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
  let trackedElement: (Element & GlobalEventHandlers) | null = null
  let initialGrabPos: Vec2 | null = null
  let lastPos: Vec2 | null = null

  const dragging = computed(() => trackedPointer.value != null)

  function doStop(e: PointerEvent) {
    if (trackedPointer.value != null) {
      trackedElement?.releasePointerCapture(trackedPointer.value)
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

  const events = {
    pointerdown(e: PointerEvent) {
      // pointers should not respond to unmasked mouse buttons
      if ((e.buttons & requiredButtonMask) === 0) {
        return
      }

      if (trackedPointer.value == null && e.currentTarget instanceof Element) {
        e.preventDefault()
        trackedPointer.value = e.pointerId
        // This is mostly SAFE, as virtually all `Element`s also extend `GlobalEventHandlers`.
        trackedElement = e.currentTarget as Element & GlobalEventHandlers
        trackedElement.setPointerCapture(e.pointerId)
        initialGrabPos = new Vec2(e.clientX, e.clientY)
        lastPos = initialGrabPos
        handler(computePosition(e, initialGrabPos, lastPos), e, 'start')
      }
    },
    pointerup(e: PointerEvent) {
      if (trackedPointer.value !== e.pointerId) {
        return
      }
      e.preventDefault()
      doStop(e)
    },
    pointermove(e: PointerEvent) {
      if (trackedPointer.value !== e.pointerId) {
        return
      }
      e.preventDefault()
      // handle release of all masked buttons as stop
      if ((e.buttons & requiredButtonMask) !== 0) {
        doMove(e)
      } else {
        doStop(e)
      }
    },
  }

  const stopEvents = {
    pointerdown(e: PointerEvent) {
      e.stopImmediatePropagation()
      events.pointerdown(e)
    },
    pointerup(e: PointerEvent) {
      e.stopImmediatePropagation()
      events.pointerup(e)
    },
    pointermove(e: PointerEvent) {
      e.stopImmediatePropagation()
      events.pointermove(e)
    },
  }

  return proxyRefs({
    events,
    stop: { events: stopEvents },
    dragging,
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
