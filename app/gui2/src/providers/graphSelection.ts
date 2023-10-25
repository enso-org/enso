import type { NavigatorComposable } from '@/util/navigator'
import type { Rect } from '@/util/rect'
import { useSelection } from '@/util/selection'
import type { ExprId } from 'shared/yjsModel'
import { createContextStore } from '.'

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
