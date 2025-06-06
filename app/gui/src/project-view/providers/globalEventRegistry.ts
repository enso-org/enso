import type { WindowEventTarget } from '@/composables/events'
import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'

interface GlobalEventRegistry {
  globalEventRegistry: WindowEventTarget
  globalEventRegistryPre: WindowEventTarget
}

const [provideRegistry, useGlobalEventRegistry] = createContextStore(
  'GlobalEvent',
  identity<GlobalEventRegistry>,
)

export { useGlobalEventRegistry }

/**
 * Create a {@link GlobalEventRegistry} and make is available to the component's children.
 * @returns the created registry
 */
export function provideGlobalEventRegistry(): GlobalEventRegistry {
  const globalEventRegistryPre = eventRegistry()
  const globalEventRegistry = eventRegistry(window, globalEventRegistryPre)
  const registry = { globalEventRegistry, globalEventRegistryPre }
  provideRegistry(registry)
  return registry
}

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
