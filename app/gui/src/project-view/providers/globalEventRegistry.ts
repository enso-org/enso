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
    const globalEventRegistry = eventRegistry(window)
    const globalEventRegistryPre: typeof globalEventRegistry = {
      addEventListener: (event, callback, options = {}) =>
        globalEventRegistry.addEventListener(event, callback, { ...options, phase: 'pre' }),
      removeEventListener: (event, callback, options = {}) =>
        globalEventRegistry.removeEventListener(event, callback, { ...options, phase: 'pre' }),
      dispatchEvent: globalEventRegistry.dispatchEvent,
    }
    return { globalEventRegistry, globalEventRegistryPre }
  },
)

export type EventRegistryPhase = 'pre' | 'main'
export type EventRegistryOptions = {
  capture: boolean
  phase: EventRegistryPhase
}

function normalizeOptions(options?: Partial<EventRegistryOptions>): EventRegistryOptions {
  return {
    capture: options?.capture ?? false,
    phase: options?.phase ?? 'main',
  }
}

function eventRegistry(source?: EventTarget) {
  type HandlerSet = Set<(e: Event) => void>
  type AllHandlerTypes = { [K in EventRegistryPhase]: HandlerSet }
  type EventRegistry = Map<keyof WindowEventMap, AllHandlerTypes>

  const registryBubble: EventRegistry = new Map()
  const registryCapture: EventRegistry = new Map()

  function addEventListener<K extends keyof WindowEventMap>(
    event: K,
    callback: (e: WindowEventMap[K]) => void,
    options?: Partial<EventRegistryOptions>,
  ) {
    const opt = normalizeOptions(options)
    const registry = opt.capture ? registryCapture : registryBubble
    const dispatcher = registry.get(event) ?? { pre: new Set(), main: new Set() }
    if (source && dispatcher.pre.size === 0 && dispatcher.main.size === 0) {
      source.addEventListener(event, dispatchEvent, { capture: opt.capture })
    }
    dispatcher[opt.phase].add(callback as (e: Event) => void)
    registry.set(event, dispatcher)
  }

  function removeEventListener<K extends keyof WindowEventMap>(
    event: K,
    callback: (e: WindowEventMap[K]) => void,
    options?: Partial<EventRegistryOptions>,
  ) {
    const opt = normalizeOptions(options)
    const registry = opt.capture ? registryCapture : registryBubble
    const dispatcher = registry.get(event)
    dispatcher?.[opt.phase].delete(callback as any)
    if (source && dispatcher && dispatcher.pre.size === 0 && dispatcher.main.size === 0) {
      source.removeEventListener(event, dispatchEvent, { capture: opt.capture })
    }
  }

  function dispatchEvent(event: Event) {
    const registry = event.eventPhase === Event.CAPTURING_PHASE ? registryCapture : registryBubble
    const handlers = registry.get(event.type as any)
    for (const handler of handlers?.pre ?? []) handler(event)
    for (const handler of handlers?.main ?? []) handler(event)
    return !event.cancelable || !event.defaultPrevented
  }

  return { addEventListener, removeEventListener, dispatchEvent }
}
