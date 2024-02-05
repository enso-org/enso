import { createContextStore } from '@/providers'
<<<<<<< HEAD
import { useGraphStore } from '@/stores/graph'
import { asNodeId } from '@/stores/graph/graphDatabase'
=======
import { type NodeId } from '@/stores/graph/graphDatabase'
>>>>>>> origin/develop
import { Ast } from '@/util/ast'
import { computed, proxyRefs, type Ref } from 'vue'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (astRoot: Ref<Ast.Ast>, nodeId: Ref<NodeId>, hasActiveAnimations: Ref<boolean>) => {
    const graph = useGraphStore()
    const nodeId = computed(() => asNodeId(astRoot.value.id))
    const nodeSpanStart = computed(() => graph.moduleSource.getSpan(astRoot.value.id)![0])
    return proxyRefs({ astRoot, nodeId, nodeSpanStart, hasActiveAnimations })
  },
)
