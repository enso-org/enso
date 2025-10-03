import { proxyRefs } from '@/util/reactivity'
import { ref } from 'vue'
import { createContextStore } from '.'

export type ActionContext = ReturnType<typeof injectActionContext>

export const [provideActionContext, injectActionContext] = createContextStore(
  'actionContext',
  () => {
    return proxyRefs({
      openPosition: ref<{ x: number; y: number } | null>(null),
    })
  },
)
