import { registerHandlers } from '@/providers/action'
import { type Node } from '@/stores/graph'
import { type ToValue } from '@/util/reactivity'
import * as iter from 'enso-common/src/utilities/data/iter'
import { computed, type ComputedRef, ref, toValue } from 'vue'

export function registerSelectionActionHandlers(
  selectedNodes: ToValue<Iterable<Node>>,
  actions: {
    collapseNodes: (nodes: Node[]) => void
    copyNodesToClipboard: (nodes: Node[]) => void
    deleteNodes: (nodes: Node[]) => void
  },
) {
  function everyNode(predicate: (node: Node) => boolean): ComputedRef<boolean> {
    return computed(() => iter.every(toValue(selectedNodes), predicate))
  }
  const selectedNodesArray = computed(() => [...toValue(selectedNodes)])
  const singleNodeSelected = computed<boolean>(() => selectedNodesArray.value.length === 1)
  const noNormalNodes = everyNode((node) => node.type !== 'component')
  function action(action: keyof typeof actions): () => void {
    return () => actions[action](toValue(selectedNodesArray))
  }
  return registerHandlers({
    'components.collapse': {
      disabled: computed(() => singleNodeSelected.value || noNormalNodes.value),
      action: action('collapseNodes'),
    },
    'components.copy': {
      disabled: noNormalNodes,
      description: computed(() =>
        singleNodeSelected.value ? 'Copy Component' : 'Copy Selected Components',
      ),
      action: action('copyNodesToClipboard'),
    },
    'components.deleteSelected': {
      disabled: noNormalNodes,
      description: computed(() =>
        singleNodeSelected.value ? 'Delete Component' : 'Delete Selected Components',
      ),
      action: action('deleteNodes'),
    },
    'components.pickColorMulti': {
      toggled: ref(false),
      disabled: computed(() => singleNodeSelected.value || noNormalNodes.value),
    },
  })
}
