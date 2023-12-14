import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import { type Ref } from 'vue'

export interface GuiConfig {
  engine?: {
    projectManagerUrl?: string
    preferredVersion?: string
    rpcUrl?: string
    dataUrl?: string
    namespace?: string
  }
  startup?: {
    project?: string
    displayedProjectName: string
  }
  window?: { topBarOffset?: string }
}
export { injectFn as injectGuiConfig, provideFn as provideGuiConfig }
const { provideFn, injectFn } = createContextStore('GUI config', identity<Ref<GuiConfig>>)
