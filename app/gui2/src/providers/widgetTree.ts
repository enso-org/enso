import { createContextStore } from '@/providers'
import { useGraphStore } from '@/stores/graph'
import { type NodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import type { Icon } from '@/util/iconName'
import { computed, proxyRefs, type Ref } from 'vue'
import type { WidgetEditHandler } from './widgetRegistry/editHandler'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (
    astRoot: Ref<Ast.Ast>,
    nodeId: Ref<NodeId>,
    nodeElement: Ref<HTMLElement | undefined>,
    icon: Ref<Icon>,
    connectedSelfArgumentId: Ref<Ast.AstId | undefined>,
    potentialSelfArgumentId: Ref<Ast.AstId | undefined>,
    conditionalPorts: Ref<Set<Ast.AstId>>,
    extended: Ref<boolean>,
    hasActiveAnimations: Ref<boolean>,
    currentEdit: Ref<WidgetEditHandler | undefined>,
    emitOpenFullMenu: () => void,
  ) => {
    const graph = useGraphStore()
    const nodeSpanStart = computed(() => graph.moduleSource.getSpan(astRoot.value.id)![0])
    return proxyRefs({
      astRoot,
      nodeId,
      nodeElement,
      icon,
      connectedSelfArgumentId,
      potentialSelfArgumentId,
      conditionalPorts,
      extended,
      nodeSpanStart,
      hasActiveAnimations,
      currentEdit,
      emitOpenFullMenu,
    })
  },
)
