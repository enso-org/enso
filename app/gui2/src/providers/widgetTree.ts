import { createContextStore } from '@/providers'
import { Ast } from '@/util/ast'
import { computed, proxyRefs, type Ref } from 'vue'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (astRoot: Ref<Ast.Ast>, hasActiveAnimations: Ref<boolean>) => {
    const nodeId = computed(() => astRoot.value.astId)
    const nodeSpanStart = computed(() => astRoot.value.astExtended!.span()[0])
    return proxyRefs({ astRoot, nodeId, nodeSpanStart, hasActiveAnimations })
  },
)
