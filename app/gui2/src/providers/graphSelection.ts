import type { NavigatorComposable } from '@/composables/navigator'
import { useSelection, type SelectionComposable } from '@/composables/selection'
import { createContextStore } from '@/providers'
import { type NodeId } from '@/stores/graph'
import type { Rect } from '@/util/data/rect'

const SELECTION_BRUSH_MARGIN_PX = 6

export type GraphSelection = SelectionComposable<NodeId>
export { injectFn as injectGraphSelection, provideFn as provideGraphSelection }
const { provideFn, injectFn } = createContextStore(
  'graph selection',
  (
    navigator: NavigatorComposable,
    nodeRects: Map<NodeId, Rect>,
    isPortEnabled,
    callbacks: {
      onSelected?: (id: NodeId) => void
      onDeselected?: (id: NodeId) => void
    } = {},
  ) => {
    return useSelection(navigator, nodeRects, isPortEnabled, SELECTION_BRUSH_MARGIN_PX, callbacks)
  },
)
