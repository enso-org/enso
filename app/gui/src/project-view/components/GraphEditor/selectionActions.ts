import { type Node } from '$/providers/openedProjects/graph'
import { toggledAction } from '@/providers/action'
import { type ToValue } from '@/util/reactivity'
import * as iter from 'enso-common/src/utilities/data/iter'
import { computed, toValue } from 'vue'

/**
 * Register handlers of actions for many selected components.
 */
export function selectionActionHandlers(
  selectedNodes: ToValue<Iterable<Node>>,
  detachingPossible: ToValue<boolean>,
  actions: {
    collapseNodes: (nodes: Node[]) => void
    copyNodesToClipboard: (nodes: Node[]) => void
    alignLeftNodes: (nodes: Node[]) => void
    alignRightNodes: (nodes: Node[]) => void
    alignTopNodes: (nodes: Node[]) => void
    alignBottomNodes: (nodes: Node[]) => void
    alignCenterNodes: (nodes: Node[]) => void
    deleteNodes: (nodes: Node[]) => void
    deleteAndConnectAround: (nodes: Node[]) => void
  },
) {
  const selectedNodesArray = computed(() => [...toValue(selectedNodes)])
  const singleNodeSelected = computed<boolean>(() => selectedNodesArray.value.length === 1)
  const multipleNodesSelected = computed<boolean>(() => selectedNodesArray.value.length > 1)
  const atLeastOneComponent = computed(() =>
    iter.some(toValue(selectedNodes), (node) => node.type === 'component'),
  )
  function action(action: keyof typeof actions): () => void {
    return () => actions[action](toValue(selectedNodesArray))
  }
  return {
    'components.collapse': {
      enabled: computed(() => multipleNodesSelected.value && atLeastOneComponent.value),
      action: action('collapseNodes'),
    },
    'components.copy': {
      enabled: atLeastOneComponent,
      description: computed(() =>
        singleNodeSelected.value ? 'Copy Component' : 'Copy Selected Components',
      ),
      action: action('copyNodesToClipboard'),
    },
    'components.deleteSelected': {
      enabled: atLeastOneComponent,
      description: computed(() =>
        singleNodeSelected.value ? 'Delete Component' : 'Delete Selected Components',
      ),
      action: action('deleteNodes'),
    },
    'components.pickColorMulti': {
      ...toggledAction(),
      enabled: computed(() => multipleNodesSelected.value && atLeastOneComponent.value),
    },
    'components.alignLeft': {
      enabled: computed(() => multipleNodesSelected.value),
      action: action('alignLeftNodes'),
    },
    'components.alignRight': {
      enabled: computed(() => multipleNodesSelected.value),
      action: action('alignRightNodes'),
    },
    'components.alignTop': {
      enabled: computed(() => multipleNodesSelected.value),
      action: action('alignTopNodes'),
    },
    'components.alignBottom': {
      enabled: computed(() => multipleNodesSelected.value),
      action: action('alignBottomNodes'),
    },
    'components.alignCenter': {
      enabled: computed(() => multipleNodesSelected.value),
      action: action('alignCenterNodes'),
    },
    'components.deleteAndConnectAround': {
      enabled: computed(() => atLeastOneComponent.value && toValue(detachingPossible)),
      description: computed(() =>
        singleNodeSelected.value ?
          'Delete and Connect Around'
        : 'Delete Selected and Connect Around',
      ),
      action: action('deleteAndConnectAround'),
    },
  }
}
