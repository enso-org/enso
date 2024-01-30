import type { BreadcrumbItem } from '@/components/NavBreadcrumbs.vue'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { AstId } from '@/util/ast/abstract.ts'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import type { StackItem } from 'shared/languageServerTypes.ts'
import { computed, onMounted, ref } from 'vue'

export function useStackNavigator() {
  const projectStore = useProjectStore()
  const graphStore = useGraphStore()

  const breadcrumbs = ref<StackItem[]>([])

  const breadcrumbLabels = computed(() => {
    const activeStackLength = projectStore.executionContext.desiredStack.length
    return breadcrumbs.value.map((item, index) => {
      const label = stackItemToLabel(item)
      const isActive = index < activeStackLength
      return { label, active: isActive } satisfies BreadcrumbItem
    })
  })

  const allowNavigationLeft = computed(() => {
    return projectStore.executionContext.desiredStack.length > 1
  })

  const allowNavigationRight = computed(() => {
    return projectStore.executionContext.desiredStack.length < breadcrumbs.value.length
  })

  function stackItemToLabel(item: StackItem): string {
    return graphStore.db.stackItemToMethodName(item) ?? 'unknown'
  }

  function handleBreadcrumbClick(index: number) {
    const activeStack = projectStore.executionContext.desiredStack
    if (index < activeStack.length) {
      const diff = activeStack.length - index - 1
      for (let i = 0; i < diff; i++) {
        projectStore.executionContext.pop()
      }
    } else if (index >= activeStack.length) {
      const diff = index - activeStack.length + 1
      for (let i = 0; i < diff; i++) {
        const stackItem = breadcrumbs.value[index - i]
        if (stackItem?.type === 'LocalCall') {
          const exprId = stackItem.expressionId
          projectStore.executionContext.push(exprId)
        } else {
          console.warn('Cannot enter non-local call.')
        }
      }
    }
    graphStore.updateState()
  }

  function enterNode(id: AstId) {
    const node = graphStore.astModule.get(id)!
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
    projectStore.executionContext.push(node.externalId)
    graphStore.updateState()
    breadcrumbs.value = projectStore.executionContext.desiredStack.slice()
  }

  function exitNode() {
    projectStore.executionContext.pop()
    graphStore.updateState()
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
    graphStore.updateState()
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
