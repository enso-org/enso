import { createContextStore } from '@/providers'
import type { AstId, TokenId } from '@/util/ast/abstract.ts'
import type { PortId } from '@/providers/portInfo.ts'
import { identity } from '@vueuse/core'
import type { RendererElement } from 'vue'

interface SelectionArrowInfo {
  /** Id of the subexpression that should display arrow underneath. */
  id: AstId | PortId | TokenId | null
  /** Child widget can call this callback to request teleport of the arrow to specified element. */
  requestArrow: (to: RendererElement) => void
  /** Whether or not the arrow provided by this context instance was already requested. Do not request it twice, it will still it from other elements! */
  handled: boolean
}

export { injectFn as injectSelectionArrow, provideFn as provideSelectionArrow }
const { provideFn, injectFn } = createContextStore('Selection arrow info', identity<SelectionArrowInfo>)
