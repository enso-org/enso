import { onScopeDispose, watch, type WatchSource } from 'vue'

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
