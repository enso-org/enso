import type { NavigatorComposable } from '@/composables/navigator'
import { useGraphHover, useSelection } from '@/composables/selection'
import { createContextStore } from '@/providers'
import { type NodeId } from '@/stores/graph'
import type { Rect } from '@/util/data/rect'
import type { ExternalId } from 'shared/yjsModel'
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
    pack: (id: NodeId) => ExternalId | undefined,
    unpack: (packed: ExternalId) => NodeId | undefined,
    callbacks: {
      onSelected?: (id: NodeId) => void
      onDeselected?: (id: NodeId) => void
    } = {},
  ) =>
    proxyRefs({
      ...useSelection(
        navigator,
        nodeRects,
        SELECTION_BRUSH_MARGIN_PX,
        isValid,
        pack,
        unpack,
        callbacks,
      ),
      ...useGraphHover(isPortEnabled),
    }),
)
