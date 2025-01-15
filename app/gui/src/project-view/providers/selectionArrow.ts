import { createContextStore } from '@/providers'
import type { PortId } from '@/providers/portInfo.ts'
import type { AstId, TokenId } from '@/util/ast/abstract.ts'
import { identity } from '@vueuse/core'
import type { RendererElement } from 'vue'

interface SelectionArrowInfo {
  /** Id of the subexpression that should display arrow underneath. */
  id: AstId | PortId | TokenId | null
  /** Child widget can call this callback to request teleport of the arrow to specified element. */
  requestArrow: (to: RendererElement) => void
  /**
   * Whether or not the arrow provided by this context instance was already requested.
   * Do not request the arrow twice, it will be stolen from other elements!
   */
  handled: boolean
  /**
   * Child widget may set this flag to suppress arrow displaying.
   *
   * A usage example is a child suppressing arrow on hover, because interactions with this child
   * will not open the drop-down (because the child is drop-down itself, for examle).
   */
  suppressArrow: boolean
}

export const [provideSelectionArrow, injectSelectionArrow] = createContextStore(
  'Selection arrow info',
  identity<SelectionArrowInfo>,
)
