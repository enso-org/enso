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

export const guiConfigProvideKey$FOR$INTERNAL$USE$ONLY = Symbol('appConfig') as InjectionKey<
  Ref<GuiConfig>
>

export function useGuiConfig(): Ref<GuiConfig> {
  const injected = inject(guiConfigProvideKey$FOR$INTERNAL$USE$ONLY)
  if (injected == null) throw new Error('AppConfig not provided')
  return injected
}

export function provideGuiConfig(appConfig: Ref<GuiConfig>) {
  provide(guiConfigProvideKey$FOR$INTERNAL$USE$ONLY, appConfig)
}
