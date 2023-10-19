import type { Rect } from '@/stores/rect'
import type { NavigatorComposable } from '@/util/navigator'
import { useSelection } from '@/util/selection'
import type { ExprId } from 'shared/yjsModel'
import { createProvidable } from '.'

export type GraphSelection = ReturnType<typeof selectionFactory>
const { provideFn, useFn } = createProvidable(selectionFactory)
export { provideFn as provideGraphSelection, useFn as useGraphSelection }

const SELECTION_BRUSH_MARGIN_PX = 6

function selectionFactory(
  navigator: NavigatorComposable,
  nodeRects: Map<ExprId, Rect>,
  callbacks: {
    onSelected?: (id: ExprId) => void
    onDeselected?: (id: ExprId) => void
  } = {},
) {
  return useSelection(navigator, nodeRects, SELECTION_BRUSH_MARGIN_PX, callbacks)
}
