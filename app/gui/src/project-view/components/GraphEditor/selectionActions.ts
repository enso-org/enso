import { toggledAction } from '@/providers/action'
import { type Node } from '@/stores/graph'
import { type ToValue } from '@/util/reactivity'
import * as iter from 'enso-common/src/utilities/data/iter'
import { computed, toValue } from 'vue'

/**
 * Register handlers of actions for many selected components.
 */
export function selectionActionHandlers(
  selectedNodes: ToValue<Iterable<Node>>,
  actions: {
    collapseNodes: (nodes: Node[]) => void
    copyNodesToClipboard: (nodes: Node[]) => void
    deleteNodes: (nodes: Node[]) => void
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
  }
}
