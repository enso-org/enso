import type { NavigatorComposable } from '@/composables/navigator'
import { useGraphHover, useSelection, type SelectionOptions } from '@/composables/selection'
import { createContextStore } from '@/providers'
import { type NodeId } from '@/stores/graph'
import type { Rect } from '@/util/data/rect'
import { proxyRefs } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'

const SELECTION_BRUSH_MARGIN_PX = 6

export const [provideGraphSelection, injectGraphSelection] = createContextStore(
  'graph selection',
  (
    navigator: NavigatorComposable,
    nodeRects: Map<NodeId, Rect>,
    isPortEnabled,
    options: SelectionOptions<NodeId, ExternalId>,
  ) =>
    proxyRefs({
      ...useSelection(navigator, nodeRects, { margin: SELECTION_BRUSH_MARGIN_PX, ...options }),
      ...useGraphHover(isPortEnabled),
    }),
)
