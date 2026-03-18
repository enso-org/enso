import { proxyRefs } from '$/utils/reactivity'
import type { Interaction } from '@/providers/interactionHandler'
import { ref, shallowRef } from 'vue'
import { createContextStore } from '.'

export type ActionContext = ReturnType<typeof injectActionContext>

interface ActionMenuControls {
  closeMenu?: () => void
  endInteraction?: () => void
}

export const [provideActionContext, injectActionContext] = createContextStore(
  'actionContext',
  (controls: ActionMenuControls = {}) => {
    return proxyRefs({
      openPosition: ref<{ x: number; y: number } | null>(null),
      menuInteraction: shallowRef<Interaction>(),
      closeMenu: controls.closeMenu ?? (() => {}),
      endInteraction: controls.endInteraction ?? (() => {}),
    })
  },
)
