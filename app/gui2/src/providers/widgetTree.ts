import { createContextStore } from '@/providers'
import { useGraphStore } from '@/stores/graph'
import { type NodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { computed, proxyRefs, type Ref } from 'vue'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (astRoot: Ref<Ast.Ast>, nodeId: Ref<NodeId>, hasActiveAnimations: Ref<boolean>) => {
    const graph = useGraphStore()
    const nodeSpanStart = computed(() => graph.moduleSource.getSpan(astRoot.value.id)![0])
    return proxyRefs({ astRoot, nodeId, nodeSpanStart, hasActiveAnimations })
  },
)
