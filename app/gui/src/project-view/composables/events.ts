/** @file Vue composables for listening to DOM events. */

import type { KeyboardComposable } from '@/composables/keyboard.ts'
import type { Opt } from '@/util/data/opt'
import { Vec2 } from '@/util/data/vec2'
import { type VueInstance } from '@vueuse/core'
import {
  computed,
  onScopeDispose,
  proxyRefs,
  ref,
  shallowRef,
  toValue,
  watch,
  watchEffect,
  type Ref,
  type ShallowRef,
  type WatchSource,
} from 'vue'
import { useRaf } from './animation'

/** TODO: Add docs */
export function isTriggeredByKeyboard(e: MouseEvent | PointerEvent) {
  if (e instanceof PointerEvent) return e.pointerType !== 'mouse'
  else return false
}

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
  handler: EventListenerOrEventListenerObject,
  options?: boolean | AddEventListenerOptions,
): void
/**
 * Add an event listener for the duration of the component's lifetime.
 * @param target element on which to register the event
 * @param event name of event to register
 * @param handler event handler
 */
export function useEvent(
  target: EventTarget,
  event: string,
  handler: EventListenerOrEventListenerObject,
  options?: boolean | AddEventListenerOptions,
): void {
  target.addEventListener(event, handler, options)
  onScopeDispose(() => target.removeEventListener(event, handler, options))
}

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
/**
 * Add an event listener for the duration of condition being true.
 * @param target element on which to register the event
 * @param event name of event to register
 * @param condition the condition that determines if event is bound
 * @param handler event handler
 * @param options listener options
 */
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
export function keyboardBusy(): boolean {
  return (
    document.activeElement !== document.body &&
    document.activeElement instanceof HTMLElement &&
    isEditable(document.activeElement)
  )
}

function isEditable(element: HTMLElement) {
  return (
    element.isContentEditable ||
    element instanceof HTMLInputElement ||
    element instanceof HTMLTextAreaElement
  )
}

/** Whether focused element is within given element's subtree. */
export function focusIsIn(el: Element | undefined | null) {
  return el && el.contains(document.activeElement)
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

/** Check if `mod` key (ctrl or cmd) appropriate for current platform is used */
export function modKey(e: KeyboardEvent | MouseEvent): boolean {
  return isMacLike ? e.metaKey : e.ctrlKey
}

/**
 * A helper for getting Element out of VueInstance, it allows using `useResizeObserver` with Vue components.
 *
 * Note that this function is only shallowly reactive: It will trigger its reactive scope if the value of `element`
 * changes, but not if the root `Element` of the provided `VueInstance` changes. This is because a
 * `ComponentPublicInstance` is implicitly treated as if `markRaw` were applied[^1]. As a result, this function should
 * not be used for any component that may have a dynamic root element; rather, the component can use `defineExpose` to
 * provide access to a `ref`.
 *
 * [^1]: https://github.com/vuejs/core/blob/ae97e5053895eeaaa443306e72cd8f45da001179/packages/runtime-core/src/componentPublicInstance.ts#L312
 */
export function unrefElement(
  element: Ref<Element | undefined | null | VueInstance>,
): Element | undefined | null {
  const plain = toValue(element)
  const result = (plain as VueInstance)?.$el ?? plain
  // A component's root can be a Node (if it's a fragment), TextNode, or Comment (if its root uses a v-if).
  if (result != null && !(result instanceof Element)) {
    if (result instanceof Comment && result.data.includes('v-if')) {
      console.warn(
        "unrefElement: Component root is a v-if, but a root element can't be watched reactively.",
        result,
      )
    }
    return undefined
  }
  return result
}

interface ResizeObserverData {
  refCount: number
  boundRectUsers: number
  contentRect: ShallowRef<Vec2>
  boundRect: ShallowRef<Vec2>
}

const resizeObserverData = new WeakMap<Element, ResizeObserverData>()
function getOrCreateObserverData(element: Element): ResizeObserverData {
  const existingData = resizeObserverData.get(element)
  if (existingData) return existingData
  const data: ResizeObserverData = {
    refCount: 0,
    boundRectUsers: 0,
    contentRect: shallowRef<Vec2>(Vec2.Zero),
    boundRect: shallowRef<Vec2>(Vec2.Zero),
  }
  resizeObserverData.set(element, data)
  return data
}

const RESIZE_OBSERVER_EPSILON = 0.01

const sharedResizeObserver: ResizeObserver | undefined =
  typeof ResizeObserver === 'undefined' ? undefined : (
    new ResizeObserver((entries) => {
      for (const entry of entries) {
        const data = resizeObserverData.get(entry.target)
        if (data != null) {
          if (entry.contentRect != null) {
            const newSize = Vec2.FromSize(entry.contentRect)
            if (!data.contentRect.value.equalsApproximately(newSize, RESIZE_OBSERVER_EPSILON))
              data.contentRect.value = newSize
          }
          if (data.boundRectUsers > 0) {
            const newSize = Vec2.FromSize(entry.target.getBoundingClientRect())
            if (!data.boundRect.value.equalsApproximately(newSize, RESIZE_OBSERVER_EPSILON))
              data.boundRect.value = newSize
          }
        }
      }
    })
  )

/**
 * Get DOM node size and keep it up to date.
 *
 * # Warning:
 * Updating DOM node layout based on values derived from their size can introduce unwanted feedback
 * loops across the script and layout reflow. Avoid doing that.
 * @param elementRef DOM node to observe.
 * @returns Reactive value with the DOM node size.
 */
export function useResizeObserver(
  elementRef: Ref<Element | undefined | null | VueInstance>,
  useContentRect = true,
): Ref<Vec2> {
  if (!sharedResizeObserver) {
    const sizeRef = shallowRef<Vec2>(Vec2.Zero)
    // Fallback implementation for browsers/test environment that do not support ResizeObserver:
    // Grab the size of the element every time the ref is assigned, or when the page is resized.
    function refreshSize() {
      const element = unrefElement(elementRef)
      if (element != null) {
        const rect = element.getBoundingClientRect()
        sizeRef.value = new Vec2(rect.width, rect.height)
      }
    }
    watchEffect(refreshSize)
    useEvent(window, 'resize', refreshSize)
    return sizeRef
  }
  const observer = sharedResizeObserver
  watchEffect((onCleanup) => {
    const element = unrefElement(elementRef)
    if (element != null) {
      const data = getOrCreateObserverData(element)
      if (data.refCount === 0) observer.observe(element)
      data.refCount += 1
      if (!useContentRect) {
        if (data.boundRectUsers === 0) {
          const rect = element.getBoundingClientRect()
          data.boundRect.value = new Vec2(rect.width, rect.height)
        }
        data.boundRectUsers += 1
      }
      onCleanup(() => {
        if (elementRef.value != null) {
          data.refCount -= 1
          if (!useContentRect) data.boundRectUsers -= 1
          if (data.refCount === 0) observer.unobserve(element)
        }
      })
    }
  })

  return computed(() => {
    const element = unrefElement(elementRef)
    if (element == null) return Vec2.Zero
    const data = getOrCreateObserverData(element)
    return useContentRect ? data.contentRect.value : data.boundRect.value
  })
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

type PointerEventType = 'start' | 'move' | 'stop' | 'cancel'

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

/** Options for `usePointer` composable. */
export interface UsePointerOptions {
  /**
   * Declare which buttons to look for. The value represents a `PointerEvent.buttons` mask.
   * Defaults to main mouse button.
   */
  requiredButtonMask?: number
  /**
   * Which element should capture pointer when drag starts: event's `target`, `currentTarget`,
   * or none.
   */
  pointerCapturedBy?: 'target' | 'currentTarget' | 'none'
  /** Additional condition for drag */
  predicate?: (e: PointerEvent) => boolean
}

/**
 * Register for a pointer dragging events.
 * @param handler callback on any pointer event. If `false` is returned from the callback, the
 * event will be considered _not_ handled and will propagate further.
 */
export function usePointer(
  handler: (pos: EventPosition, event: PointerEvent, eventType: PointerEventType) => void | boolean,
  options: UsePointerOptions = {},
) {
  const requiredButtonMask = options.requiredButtonMask ?? PointerButtonMask.Main
  const pointerCapturedBy = options.pointerCapturedBy ?? 'currentTarget'
  const predicate = options.predicate ?? ((_e) => true)
  const dragState = ref<
    | {
        trackedPointer: number
        trackedElement: Element | undefined
        initialGrabPos: Vec2
        lastPos: Vec2
      }
    | undefined
  >()

  const dragging = computed(() => dragState.value != null)

  function doStop(e: PointerEvent, eventType: PointerEventType = 'stop') {
    if (dragState.value?.trackedPointer !== e.pointerId) return
    const { trackedElement, trackedPointer, initialGrabPos, lastPos } = dragState.value
    trackedElement?.releasePointerCapture(trackedPointer)

    if (handler(computePosition(e, initialGrabPos, lastPos), e, eventType) !== false) {
      e.stopImmediatePropagation()
    }

    dragState.value = undefined
  }

  function doMove(e: PointerEvent) {
    if (dragState.value?.trackedPointer !== e.pointerId) return
    const { initialGrabPos, lastPos } = dragState.value

    if (handler(computePosition(e, initialGrabPos, lastPos), e, 'move') !== false) {
      e.stopImmediatePropagation()
    }
    dragState.value.lastPos = new Vec2(e.clientX, e.clientY)
  }

  const events = {
    pointerdown(e: PointerEvent) {
      // pointers should not respond to unmasked mouse buttons
      if ((e.buttons & requiredButtonMask) === 0 || !predicate(e)) return
      if (dragState.value != null) return

      let trackedElement: Element | undefined
      const trackedTarget =
        pointerCapturedBy === 'currentTarget' ? e.currentTarget
        : pointerCapturedBy === 'target' ? e.target
        : null
      if (trackedTarget instanceof Element) {
        // `setPointerCapture` is not defined in tests.
        trackedTarget?.setPointerCapture?.(e.pointerId)
        trackedElement = trackedTarget
      }
      const initialGrabPos = new Vec2(e.clientX, e.clientY)
      const lastPos = initialGrabPos
      dragState.value = {
        trackedPointer: e.pointerId,
        initialGrabPos,
        lastPos,
        trackedElement,
      }
      if (handler(computePosition(e, initialGrabPos, lastPos), e, 'start') !== false) {
        e.stopImmediatePropagation()
      }
    },
    pointerup(e: PointerEvent) {
      if (dragState.value?.trackedPointer !== e.pointerId) return
      doStop(e)
    },
    pointermove(e: PointerEvent) {
      // handle release of all masked buttons as stop
      if ((e.buttons & requiredButtonMask) !== 0) {
        doMove(e)
      } else {
        doStop(e)
      }
    },
    pointercancel(e: PointerEvent) {
      if (dragState.value?.trackedPointer !== e.pointerId) return
      doStop(e, 'cancel')
    },
  }

  return proxyRefs({
    events,
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

type ArrowKey = 'ArrowLeft' | 'ArrowUp' | 'ArrowRight' | 'ArrowDown'
type PressedKeys = Record<ArrowKey, boolean>
function isArrowKey(key: string): key is ArrowKey {
  return key === 'ArrowLeft' || key === 'ArrowUp' || key === 'ArrowRight' || key === 'ArrowDown'
}

/** Options for `useArrows` composable. */
export interface UseArrowsOptions {
  /** The velocity expressed in pixels per second. Defaults to 200. */
  velocity?: number
  /** Additional condition for move. */
  predicate?: (e: KeyboardEvent) => boolean
}

/**
 * Register for arrows navigating events.
 *
 * For simplicity, the handler API is very similar to `usePointer`, but the initial position will
 * always be Vec2.Zero (and thus, the absolute and relative positions will be equal).
 *
 * The "drag" starts on first arrow keypress and ends with last arrow key release.
 * @param handler callback on any event. The 'move' event is fired on every frame, and thus does
 * not have any event associated (`event` parameter will be undefined).
 */
export function useArrows(
  handler: (
    pos: EventPosition,
    eventType: PointerEventType,
    event?: KeyboardEvent,
  ) => void | boolean,
  options: UseArrowsOptions = {},
) {
  const velocity = options.velocity ?? 200.0
  const predicate = options.predicate ?? ((_) => true)
  const clearedKeys: PressedKeys = {
    ArrowLeft: false,
    ArrowUp: false,
    ArrowRight: false,
    ArrowDown: false,
  }
  const pressedKeys: Ref<PressedKeys> = ref({ ...clearedKeys })
  const moving = computed(
    () =>
      pressedKeys.value.ArrowLeft ||
      pressedKeys.value.ArrowUp ||
      pressedKeys.value.ArrowRight ||
      pressedKeys.value.ArrowDown,
  )
  const v = computed(
    () =>
      new Vec2(
        (pressedKeys.value.ArrowLeft ? -velocity : 0) +
          (pressedKeys.value.ArrowRight ? velocity : 0),
        (pressedKeys.value.ArrowUp ? -velocity : 0) + (pressedKeys.value.ArrowDown ? velocity : 0),
      ),
  )
  const referencePoint = ref({
    t: 0,
    position: Vec2.Zero,
  })
  const lastPosition = ref(Vec2.Zero)

  const positionAt = (t: number) =>
    referencePoint.value.position.add(v.value.scale((t - referencePoint.value.t) / 1000.0))

  const callHandler = (
    t: number,
    eventType: PointerEventType,
    event?: KeyboardEvent,
    offset: Vec2 = positionAt(t),
  ) => {
    const delta = offset.sub(lastPosition.value)
    lastPosition.value = offset
    const positions = {
      initial: Vec2.Zero,
      absolute: offset,
      relative: offset,
      delta,
    }
    if (handler(positions, eventType, event) !== false && event) {
      event.stopImmediatePropagation()
      event.preventDefault()
    }
  }

  useRaf(moving, (t, _) => callHandler(t, 'move'))
  const events = {
    keydown(e: KeyboardEvent) {
      const starting = !moving.value
      if (e.repeat || !isArrowKey(e.key) || (starting && !predicate(e))) return
      referencePoint.value = {
        position: starting ? Vec2.Zero : positionAt(e.timeStamp),
        t: e.timeStamp,
      }
      pressedKeys.value[e.key] = true
      if (starting) {
        lastPosition.value = Vec2.Zero
        callHandler(e.timeStamp, 'start', e, referencePoint.value.position)
      }
    },
    focusout() {
      // Each focus change may make us miss some events, so it's safer to just cancel the movement.
      pressedKeys.value = { ...clearedKeys }
    },
  }
  useEvent(
    window,
    'keyup',
    (e) => {
      if (e.repeat) return
      if (!moving.value) return
      if (!isArrowKey(e.key)) return
      referencePoint.value = {
        position: positionAt(e.timeStamp),
        t: e.timeStamp,
      }
      pressedKeys.value[e.key] = false
      if (!moving.value) callHandler(e.timeStamp, 'stop', e, referencePoint.value.position)
    },
    { capture: true },
  )

  return { events, moving }
}

/**
 * Supports panning or zooming "capturing" wheel events.
 *
 * While events are captured, further events of the same type will continue the pan/zoom action.
 * The capture expires if no events are received within the specified `captureDurationMs`.
 * A trackpad capture also expires if any pointer movement occurs.
 */
export function useWheelActions(
  keyboard: KeyboardComposable,
  captureDurationMs: number,
  onZoom: (e: WheelEvent, inputType: 'trackpad' | 'wheel') => boolean | void,
  onPan: (e: WheelEvent) => boolean | void,
) {
  let prevEventPanInfo:
    | ({ expiration: number } & (
        | { type: 'trackpad-zoom' }
        | { type: 'wheel-zoom' }
        | { type: 'pan'; trackpad: boolean }
      ))
    | undefined = undefined

  type WheelEventType = 'trackpad-zoom' | 'wheel-zoom' | 'pan'
  function classifyEvent(e: WheelEvent): WheelEventType {
    if (e.ctrlKey) {
      // A pinch gesture is represented by setting `e.ctrlKey`. It can be distinguished from an actual Ctrl+wheel
      // combination because the real Ctrl key emits keyup/keydown events.
      const isGesture = !keyboard.ctrl
      return isGesture ? 'trackpad-zoom' : 'wheel-zoom'
    } else {
      return 'pan'
    }
  }

  function handleWheel(e: WheelEvent) {
    const newType = classifyEvent(e)
    if (e.eventPhase === e.CAPTURING_PHASE) {
      if (newType !== prevEventPanInfo?.type || e.timeStamp > prevEventPanInfo.expiration) {
        prevEventPanInfo = undefined
        return
      }
    }
    const expiration = e.timeStamp + captureDurationMs
    if (newType === 'wheel-zoom') {
      prevEventPanInfo = { expiration, type: newType }
      onZoom(e, 'wheel')
    } else if (newType === 'trackpad-zoom') {
      prevEventPanInfo = { expiration, type: newType }
      onZoom(e, 'trackpad')
    } else if (newType === 'pan') {
      const alreadyKnownTrackpad = prevEventPanInfo?.type === 'pan' && prevEventPanInfo.trackpad
      prevEventPanInfo = {
        expiration,
        type: newType,
        // Heuristic: Trackpad panning is usually multi-axis; wheel panning is not.
        trackpad: alreadyKnownTrackpad || (e.deltaX !== 0 && e.deltaY !== 0),
      }
      onPan(e)
    }
    e.preventDefault()
    e.stopPropagation()
  }

  function pointermove() {
    // If a `pointermove` event occurs, any trackpad action has ended.
    if (
      prevEventPanInfo?.type === 'trackpad-zoom' ||
      (prevEventPanInfo?.type === 'pan' && prevEventPanInfo.trackpad)
    ) {
      prevEventPanInfo = undefined
    }
  }

  return {
    events: {
      wheel: handleWheel,
    },
    captureEvents: {
      pointermove,
      wheel: handleWheel,
    },
  }
}
