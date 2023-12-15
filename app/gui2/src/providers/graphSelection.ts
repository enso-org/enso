import type { NavigatorComposable } from '@/composables/navigator'
import { useSelection } from '@/composables/selection'
import { createContextStore } from '@/providers'
import type { Rect } from '@/util/data/rect'
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
