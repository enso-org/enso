import { createContextStore } from '@/providers'
import type { AstId, TokenId } from '@/util/ast/abstract.ts'
import type { PortId } from '@/providers/portInfo.ts'
import { identity } from '@vueuse/core'
import type { RendererElement } from 'vue'

interface SelectionArrowInfo {
  id: AstId | PortId | TokenId | null
  requestArrow: (to: RendererElement) => void
}

export { injectFn as injectSelectionArrow, provideFn as provideSelectionArrow }
const { provideFn, injectFn } = createContextStore('Selection arrow info', identity<SelectionArrowInfo>)
