import { baseConfig, configValue, mergeConfig, type ApplicationConfigValue } from '@/util/config'
import { proxyRefs } from '@/util/reactivity'
import { urlParams } from '@/util/urlParams'
import { createGlobalState } from '@vueuse/core'
import { computed } from 'vue'

function resolveEnvUrl(url: string | undefined) {
  return url?.replace('__HOSTNAME__', window.location.hostname)
}

export type GuiConfig = ReturnType<typeof injectGuiConfig>
export const injectGuiConfig = createGlobalState(() => {
  const appConfig = computed(() =>
    mergeConfig(baseConfig, urlParams(), {
      onUnrecognizedOption: (p) => {
        const filtered = p.filter((p) => !p.startsWith('cloud-ide'))

        if (filtered.length > 0) {
          console.warn('Unrecognized option:', filtered)
        }
      },
    }),
  )

  const appConfigValue = computed((): ApplicationConfigValue => configValue(appConfig.value))

  const ydocUrl = computed(
    () => (appConfigValue.value.engine.ydocUrl || resolveEnvUrl($config.YDOC_SERVER_URL)) ?? null,
  )

  const projectManagerUrl = computed(
    () =>
      (appConfigValue.value.engine.projectManagerUrl ||
        resolveEnvUrl($config.PROJECT_MANAGER_URL)) ??
      null,
  )
  return proxyRefs({
    params: appConfigValue,
    ydocUrl,
    projectManagerUrl,
  })
})
