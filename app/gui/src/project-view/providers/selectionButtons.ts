import { graphBindings } from '@/bindings'
import { createContextStore } from '@/providers'
import { type ComponentButtons, injectComponentButtons } from '@/providers/componentButtons'
import { type Node } from '@/stores/graph'
import { type Button, reactiveButton } from '@/util/button'
import { type ToValue } from '@/util/reactivity'
import * as iter from 'enso-common/src/utilities/data/iter'
import { type DisjointKeysUnion } from 'enso-common/src/utilities/data/object'
import { computed, type ComputedRef, type Ref, ref, toValue } from 'vue'

export type SelectionButtons = Record<
  'collapse' | 'copy' | 'deleteSelected' | 'pickColorMulti',
  Button<void>
>

function useSelectionButtons(
  selectedNodes: ToValue<Iterable<Node>>,
  actions: {
    collapseNodes: (nodes: Node[]) => void
    copyNodesToClipboard: (nodes: Node[]) => void
    deleteNodes: (nodes: Node[]) => void
  },
): { selectedNodeCount: Readonly<Ref<number>>; buttons: SelectionButtons } {
  function everyNode(predicate: (node: Node) => boolean): ComputedRef<boolean> {
    return computed(() => iter.every(toValue(selectedNodes), predicate))
  }
  const selectedNodesArray = computed(() => [...toValue(selectedNodes)])
  const selectedNodeCount = computed<number>(() => toValue(selectedNodesArray).length)
  const singleNodeSelected = computed<boolean>(() => selectedNodeCount.value === 1)
  const noNormalNodes = everyNode((node) => node.type !== 'component')
  function action(action: keyof typeof actions): () => void {
    return () => actions[action](toValue(selectedNodesArray))
  }
  return {
    selectedNodeCount,
    buttons: {
      collapse: reactiveButton({
        disabled: computed(() => singleNodeSelected.value || noNormalNodes.value),
        icon: 'group',
        description: 'Group Selected Components',
        shortcut: graphBindings.bindings.collapse,
        action: action('collapseNodes'),
      }),
      copy: reactiveButton({
        disabled: noNormalNodes,
        icon: 'copy2',
        description: computed(() =>
          singleNodeSelected.value ? 'Copy Component' : 'Copy Selected Components',
        ),
        shortcut: graphBindings.bindings.copyNode,
        action: action('copyNodesToClipboard'),
      }),
      deleteSelected: reactiveButton({
        disabled: noNormalNodes,
        icon: 'trash2',
        description: computed(() =>
          singleNodeSelected.value ? 'Delete Component' : 'Delete Selected Components',
        ),
        shortcut: graphBindings.bindings.deleteSelected,
        action: action('deleteNodes'),
        testid: 'removeNode',
      }),
      pickColorMulti: reactiveButton({
        state: ref(false),
        disabled: computed(() => singleNodeSelected.value || noNormalNodes.value),
        icon: 'paint_palette',
        description: 'Color Selected Components',
      }),
    },
  }
}

export { injectFn as injectSelectionButtons, provideFn as provideSelectionButtons }
const [provideFn, injectFn] = createContextStore('Selection buttons', useSelectionButtons)

export type ComponentAndSelectionButtons = DisjointKeysUnion<ComponentButtons, SelectionButtons>

/** Returns {@link Button}s for the single-component actions and the selected-components actions. */
export function injectComponentAndSelectionButtons(): {
  selectedNodeCount: Readonly<Ref<number>>
  buttons: ComponentAndSelectionButtons
} {
  const selectionButtons = injectFn()
  const componentButtons = injectComponentButtons()
  return {
    ...selectionButtons,
    buttons: { ...selectionButtons.buttons, ...componentButtons },
  }
}
