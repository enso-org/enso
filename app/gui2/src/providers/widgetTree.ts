import { createContextStore } from '@/providers'
import { useGraphStore } from '@/stores/graph'
import { type NodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import type { Icon } from '@/util/iconName'
import { computed, proxyRefs, type Ref } from 'vue'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (
    astRoot: Ref<Ast.Ast>,
    nodeId: Ref<NodeId>,
    icon: Ref<Icon>,
    connectedSelfArgumentId: Ref<Ast.AstId | undefined>,
    potentialSelfArgumentId: Ref<Ast.AstId | undefined>,
    conditionalPorts: Ref<Set<Ast.AstId>>,
    onTargetablePortsChanged: () => void,
    extended: Ref<boolean>,
    hasActiveAnimations: Ref<boolean>,
    emitOpenFullMenu: () => void,
  ) => {
    const graph = useGraphStore()
    const nodeSpanStart = computed(() => graph.moduleSource.getSpan(astRoot.value.id)![0])
    return proxyRefs({
      astRoot,
      nodeId,
      icon,
      connectedSelfArgumentId,
      potentialSelfArgumentId,
      conditionalPorts,
      emitTargetablePortsChanged: onTargetablePortsChanged,
      extended,
      nodeSpanStart,
      hasActiveAnimations,
      emitOpenFullMenu,
    })
  },
)
