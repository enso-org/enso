import { createContextStore } from '@/providers'
import type { AstId, TokenId } from '@/util/ast/abstract'
import { identity } from '@vueuse/core'

declare const portIdBrand: unique symbol
/**
 * Port identification. A port represents a fragment of code displayed/modified by the widget;
 * usually Ast nodes, but other ids are also possible (like argument placeholders).
 */
export type PortId = AstId | TokenId | (string & { [portIdBrand]: never })

/**
 * Create a synthetic Port ID derived from an existing parent ID and arbitrary index. Intended for
 * creating stable identities for placeholder ports.
 */
export function syntheticPortId(base: PortId, key: string | number): PortId {
  return `:${base}[${key}]` as PortId
}

interface PortInfo {
  portId: PortId
  /** Has a persisted connection in the graph database. */
  hasPersistedConnection: boolean
  /** An edge visually points to this port, e.g. when hovering with mouse. */
  isVisualTarget: boolean
}

export const [providePortInfo, injectPortInfo] = createContextStore('Port info', identity<PortInfo>)
