import { createContextStore } from '@/providers'
import type { ApplicationConfigValue } from '@/util/config'
import { computed, proxyRefs, type Ref } from 'vue'

function resolveEnvUrl(url: string | undefined) {
  return url?.replace('__HOSTNAME__', window.location.hostname)
}

export type GuiConfig = ReturnType<typeof injectGuiConfig>
export const [provideGuiConfig, injectGuiConfig] = createContextStore(
  'GUI config',
  (config: Ref<ApplicationConfigValue>) => {
    const ydocUrl = computed(
      () => (config.value.engine.ydocUrl || resolveEnvUrl($config.YDOC_SERVER_URL)) ?? null,
    )

    const projectManagerUrl = computed(
      () =>
        (config.value.engine.projectManagerUrl || resolveEnvUrl($config.PROJECT_MANAGER_URL)) ??
        null,
    )
    return proxyRefs({
      params: config,
      ydocUrl,
      projectManagerUrl,
    })
  },
)
