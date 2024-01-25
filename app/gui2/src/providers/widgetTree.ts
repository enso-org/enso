import { createContextStore } from '@/providers'
import { asNodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { computed, proxyRefs, type Ref } from 'vue'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (astRoot: Ref<Ast.Ast>, hasActiveAnimations: Ref<boolean>) => {
    const nodeId = computed(() => asNodeId(astRoot.value.id))
    const nodeSpanStart = computed(() => astRoot.value.span![0])
    return proxyRefs({ astRoot, nodeId, nodeSpanStart, hasActiveAnimations })
  },
)
