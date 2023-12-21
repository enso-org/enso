import { createContextStore } from '@/providers'
import type { AnyWidget } from '@/providers/widgetRegistry'
import { GetUsageKey } from '@/providers/widgetUsageInfo'
import { identity } from '@vueuse/core'

interface PortInfo {
  portId: string
  connected: boolean
}

export { injectFn as injectPortInfo, provideFn as providePortInfo }
const { provideFn, injectFn } = createContextStore('Port info', identity<PortInfo>)

/**
 * Widget input type that can be used to force a specific AST to be rendered as a port widget,
 * even if it wouldn't normally be rendered as such.
 */
export class ForcePort {
  constructor(public input: AnyWidget) {
    if (input instanceof ForcePort) throw new Error('ForcePort cannot be nested')
  }
  [GetUsageKey]() {
    return this.input
  }
}
