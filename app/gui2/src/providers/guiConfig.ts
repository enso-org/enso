import { inject, provide, type InjectionKey, type Ref } from 'vue'

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
  }
  window?: { topBarOffset?: string }
}

const provideKey = Symbol('appConfig') as InjectionKey<Ref<GuiConfig>>

export function useGuiConfig(): Ref<GuiConfig> {
  const injected = inject(provideKey)
  if (injected == null) throw new Error('AppConfig not provided')
  return injected
}

export function provideGuiConfig(appConfig: Ref<GuiConfig>) {
  provide(provideKey, appConfig)
}
