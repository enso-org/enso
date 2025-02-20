import { type NodeId } from '@/stores/graph'
import { type GraphDb } from '@/stores/graph/graphDatabase'
import { type ToValue } from '@/util/reactivity'
import { computed, toValue } from 'vue'

/** Returns the component's base color, and information about state that may modify it. */
export function useComponentColors(
  graphDb: GraphDb,
  nodeSelection: { isSelected: (nodeId: NodeId) => boolean } | undefined,
  nodeId: ToValue<NodeId | undefined>,
) {
  const nodeIdValue = computed(() => toValue(nodeId))
  const node = computed(() => nodeIdValue.value && graphDb.nodeIdToNode.get(nodeIdValue.value))
  const expressionInfo = computed(
    () => node.value && graphDb.getExpressionInfo(node.value.innerExpr.externalId),
  )
  const executionState = computed(() => expressionInfo.value?.payload.type ?? 'Unknown')
  return {
    baseColor: computed(() => nodeIdValue.value && graphDb.getNodeColorStyle(nodeIdValue.value)),
    selected: computed(
      () => (nodeIdValue.value && nodeSelection?.isSelected(nodeIdValue.value)) ?? false,
    ),
    pending: computed(
      () => executionState.value === 'Unknown' || executionState.value === 'Pending',
    ),
  }
}
