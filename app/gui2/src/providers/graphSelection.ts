import type { NavigatorComposable } from '@/composables/navigator'
import { useGraphHover, useSelection } from '@/composables/selection'
import { createContextStore } from '@/providers'
import { type NodeId } from '@/stores/graph'
import type { Rect } from '@/util/data/rect'
import { proxyRefs } from 'vue'

const SELECTION_BRUSH_MARGIN_PX = 6

export { injectFn as injectGraphSelection, provideFn as provideGraphSelection }
const { provideFn, injectFn } = createContextStore(
  'graph selection',
  (
    navigator: NavigatorComposable,
    nodeRects: Map<NodeId, Rect>,
    isPortEnabled,
    isValid: (id: NodeId) => boolean,
    callbacks: {
      onSelected?: (id: NodeId) => void
      onDeselected?: (id: NodeId) => void
    } = {},
  ) =>
    proxyRefs({
      ...useSelection(navigator, nodeRects, SELECTION_BRUSH_MARGIN_PX, isValid, callbacks),
      ...useGraphHover(isPortEnabled),
    }),
)
