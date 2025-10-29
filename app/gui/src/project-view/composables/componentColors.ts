import type { NodeId } from '$/providers/openedProjects/graph'
import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import type { Opt } from '@/util/data/opt'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue } from 'vue'

/** Returns the component's base color, and information about state that may modify it. */
export function useComponentColors(
  graphDb: ToValue<Opt<GraphDb>>,
  nodeSelection: { isSelected: (nodeId: NodeId) => boolean } | undefined,
  nodeId: ToValue<NodeId | undefined>,
) {
  const nodeIdValue = computed(() => toValue(nodeId))
  const node = computed(
    () => nodeIdValue.value && toValue(graphDb)?.nodeIdToNode.get(nodeIdValue.value),
  )
  const expressionInfo = computed(
    () => node.value && toValue(graphDb)?.getExpressionInfo(node.value.innerExpr.externalId),
  )
  const executionState = computed(() => expressionInfo.value?.payload.type ?? 'Unknown')
  return {
    baseColor: computed(
      () => nodeIdValue.value && toValue(graphDb)?.getNodeColorStyle(nodeIdValue.value),
    ),
    selected: computed(
      () => (nodeIdValue.value && nodeSelection?.isSelected(nodeIdValue.value)) ?? false,
    ),
    pending: computed(
      () => executionState.value === 'Unknown' || executionState.value === 'Pending',
    ),
  }
}
