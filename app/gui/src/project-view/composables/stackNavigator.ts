import type { BreadcrumbItem } from '@/components/NavBreadcrumbs.vue'
import { type GraphStore, type NodeId } from '@/stores/graph'
import { type ProjectStore } from '@/stores/project'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import { computed, onMounted, ref } from 'vue'
import { methodPointerEquals, type StackItem } from 'ydoc-shared/languageServerTypes'

/** TODO: Add docs */
export function useStackNavigator(projectStore: ProjectStore, graphStore: GraphStore) {
  const breadcrumbs = ref<StackItem[]>([])

  const breadcrumbLabels = computed(() => {
    const activeStackLength = projectStore.executionContext.desiredStack.length
    return breadcrumbs.value.map((item, index) => {
      const label = stackItemToLabel(item, index === 0)
      const isActive = index < activeStackLength
      const isCurrentTop = index == activeStackLength - 1
      return { label, active: isActive, isCurrentTop } satisfies BreadcrumbItem
    })
  })

  const allowNavigationLeft = computed(() => {
    return projectStore.executionContext.desiredStack.length > 1
  })

  const allowNavigationRight = computed(() => {
    return projectStore.executionContext.desiredStack.length < breadcrumbs.value.length
  })

  function isProjectEntryPoint(item: StackItem) {
    return (
      item.type === 'ExplicitCall' &&
      methodPointerEquals(item.methodPointer, projectStore.entryPoint)
    )
  }

  function stackItemToLabel(item: StackItem, isStackRoot: boolean): string {
    if (isStackRoot && isProjectEntryPoint(item)) return projectStore.displayName
    const methodName = graphStore.db.stackItemToMethodName(item)
    return methodName ?? 'unknown'
  }

  function handleBreadcrumbClick(index: number) {
    projectStore.executionContext.desiredStack = breadcrumbs.value.slice(0, index + 1)
  }

  function enterNode(id: NodeId) {
    const expressionInfo = graphStore.db.getExpressionInfo(id)
    if (expressionInfo == null || expressionInfo.methodCall == null) {
      console.debug('Cannot enter node that has no method call.')
      return
    }
    const definedOnType = tryQualifiedName(expressionInfo.methodCall.methodPointer.definedOnType)
    if (!projectStore.modulePath?.ok) {
      console.warn('Cannot enter node while no module is open.')
      return
    }
    const openModuleName = qnLastSegment(projectStore.modulePath.value)
    if (definedOnType.ok && qnLastSegment(definedOnType.value) !== openModuleName) {
      console.debug('Cannot enter node that is not defined on current module.')
      return
    }
    projectStore.executionContext.push(id)
    breadcrumbs.value = projectStore.executionContext.desiredStack.slice()
  }

  function exitNode() {
    projectStore.executionContext.pop()
  }

  /// Enter the next node from the history stack. This is the node that is the first greyed out item in the breadcrumbs.
  function enterNextNodeFromHistory() {
    const nextNodeIndex = projectStore.executionContext.desiredStack.length
    const nextNode = breadcrumbs.value[nextNodeIndex]
    if (nextNode?.type !== 'LocalCall') {
      console.warn('Cannot enter non-local call.')
      return
    }
    projectStore.executionContext.push(nextNode.expressionId)
  }

  onMounted(() => {
    breadcrumbs.value = projectStore.executionContext.desiredStack.slice()
  })

  return {
    breadcrumbs,
    breadcrumbLabels,
    allowNavigationLeft,
    allowNavigationRight,
    handleBreadcrumbClick,
    enterNode,
    exitNode,
    enterNextNodeFromHistory,
  }
}
