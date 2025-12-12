import { type NodeId } from '$/providers/openedProjects/graph'
import type { ConnectedEdge } from '$/providers/openedProjects/graph/graph'
import type { NavigatorComposable } from '@/composables/navigator'
import { useGraphHover, useSelection, type SelectionOptions } from '@/composables/selection'
import { createContextStore } from '@/providers'
import type { Rect } from '@/util/data/rect'
import { proxyRefs } from '@/util/reactivity'
import { ref } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'

const SELECTION_BRUSH_MARGIN_PX = 6

export const [provideGraphSelection, useGraphSelection] = createContextStore(
  'graph selection',
  (
    navigator: NavigatorComposable,
    nodeRects: Map<NodeId, Rect>,
    isPortEnabled,
    options: SelectionOptions<NodeId, ExternalId>,
  ) => {
    const nodeSelection = useSelection(navigator, nodeRects, {
      margin: SELECTION_BRUSH_MARGIN_PX,
      ...options,
      onSelected: (...args) => {
        selectedEdge.value = undefined
        options.onSelected?.(...args)
      },
    })
    const selectedEdge = ref<ConnectedEdge>()
    return proxyRefs({
      ...nodeSelection,
      deselectAll: () => {
        selectedEdge.value = undefined
        nodeSelection.deselectAll()
      },
      selectedEdge,
      ...useGraphHover(isPortEnabled),
    })
  },
)
