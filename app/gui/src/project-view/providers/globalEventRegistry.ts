import type { WindowEventTarget } from '@/composables/events'
import { createContextStore } from '@/providers'

export interface GlobalEventRegistry {
  /** The main registry for global event handlers. */
  globalEventRegistry: WindowEventTarget
  /** Maintains event handlers to be run before the handlers set in {@link globalEventRegistry}. */
  globalEventRegistryPre: WindowEventTarget
}

/**
 * Registry for capture-mode event handlers for the `window` object. This is used instead of
 * attaching handlers to `window` directly to enable controlling the order of handlers.
 */
export const [provideGlobalEventRegistry, useGlobalEventRegistry] = createContextStore(
  'GlobalEvent',
  (): GlobalEventRegistry => {
    const globalEventRegistryPre = eventRegistry()
    const globalEventRegistry = eventRegistry(window, globalEventRegistryPre)
    return { globalEventRegistry, globalEventRegistryPre }
  },
)

function eventRegistry(source?: EventTarget, pre?: EventTarget): WindowEventTarget {
  const registry = new Map<keyof WindowEventMap, Set<(e: Event) => void>>()

  function addEventListener<K extends keyof WindowEventMap>(
    event: K,
    callback: (e: WindowEventMap[K]) => void,
  ) {
    const handlers = registry.get(event) ?? new Set()
    handlers.add(callback as any)
    if (source && !registry.has(event))
      source.addEventListener(event, dispatchEvent, { capture: true })
    registry.set(event, handlers)
  }

  function removeEventListener<K extends keyof WindowEventMap>(
    event: K,
    callback: (e: WindowEventMap[K]) => void,
  ) {
    const dispatcher = registry.get(event)
    dispatcher?.delete(callback as any)
    if (source && dispatcher?.size === 0) {
      source.removeEventListener(event, dispatchEvent)
      registry.delete(event)
    }
  }

  function dispatchEvent(event: Event) {
    if (pre) pre.dispatchEvent(event)
    const handlers = registry.get(event.type as any)
    for (const handler of handlers ?? []) handler(event)
    return !event.cancelable || !event.defaultPrevented
  }

  return { addEventListener, removeEventListener, dispatchEvent }
}
