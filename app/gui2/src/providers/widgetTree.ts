import { createContextStore } from '@/providers'
import { type NodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { computed, proxyRefs, type Ref } from 'vue'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (astRoot: Ref<Ast.Ast>, nodeId: Ref<NodeId>, hasActiveAnimations: Ref<boolean>) => {
    const nodeSpanStart = computed(() => astRoot.value.span![0])
    return proxyRefs({ astRoot, nodeId, nodeSpanStart, hasActiveAnimations })
  },
)
