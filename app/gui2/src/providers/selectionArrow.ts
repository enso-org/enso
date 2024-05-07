import { createContextStore } from '@/providers'
import type { AstId, TokenId } from '@/util/ast/abstract.ts'
import type { PortId } from '@/providers/portInfo.ts'
import { identity } from '@vueuse/core'

interface SelectionArrowInfo {
  id: AstId | PortId | TokenId
  hovered: boolean
}

export { injectFn as injectSelectionArrow, provideFn as provideSelectionArrow }
const { provideFn, injectFn } = createContextStore('Selection arrow info', identity<SelectionArrowInfo>)
