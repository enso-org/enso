import type { AstExtended } from '@/util/ast'
import { computed, proxyRefs, type Ref } from 'vue'
import { createContextStore } from '.'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (astRoot: Ref<AstExtended>, hasActiveAnimations: Ref<boolean>) => {
    const nodeId = computed(() => astRoot.value.astId)
    const nodeSpanStart = computed(() => astRoot.value.span()[0])
    return proxyRefs({ astRoot, nodeId, nodeSpanStart, hasActiveAnimations })
  },
)
