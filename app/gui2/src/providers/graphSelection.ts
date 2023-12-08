import { createContextStore } from '@/providers'
import type { Rect } from '@/util/rect'
import type { NavigatorComposable } from '@/util/vue/navigator'
import { useSelection } from '@/util/vue/selection'
import type { ExprId } from 'shared/yjsModel'

const SELECTION_BRUSH_MARGIN_PX = 6

export type GraphSelection = ReturnType<typeof injectFn>
export { injectFn as injectGraphSelection, provideFn as provideGraphSelection }
const { provideFn, injectFn } = createContextStore(
  'graph selection',
  (
    navigator: NavigatorComposable,
    nodeRects: Map<ExprId, Rect>,
    callbacks: {
      onSelected?: (id: ExprId) => void
      onDeselected?: (id: ExprId) => void
    } = {},
  ) => {
    return useSelection(navigator, nodeRects, SELECTION_BRUSH_MARGIN_PX, callbacks)
  },
)
