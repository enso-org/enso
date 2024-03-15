import type { NavigatorComposable } from '@/composables/navigator'
import { useSelection } from '@/composables/selection'
import { createContextStore } from '@/providers'
import { type NodeId } from '@/stores/graph'
import type { Rect } from '@/util/data/rect'

const SELECTION_BRUSH_MARGIN_PX = 6

export type GraphSelection = ReturnType<typeof injectFn>
export { injectFn as injectGraphSelection, provideFn as provideGraphSelection }
const { provideFn, injectFn } = createContextStore(
  'graph selection',
  (
    navigator: NavigatorComposable,
    nodeRects: Map<NodeId, Rect>,
    callbacks: {
      onSelected?: (id: NodeId) => void
      onDeselected?: (id: NodeId) => void
    } = {},
  ) => {
    return useSelection(navigator, nodeRects, SELECTION_BRUSH_MARGIN_PX, callbacks)
  },
)
