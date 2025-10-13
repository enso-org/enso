import { proxyRefs } from '@/util/reactivity'
import { createGlobalState } from '@vueuse/core'
import { parseWebAppOptionsFromSearchParams, type Options } from 'enso-common/src/options'
import { computed } from 'vue'

function resolveEnvUrl(url: string | undefined) {
  return url?.replace('__HOSTNAME__', window.location.hostname)
}

type FullWebArgs = Options
export type GuiConfig = ReturnType<typeof injectGuiConfig>
export const injectGuiConfig = createGlobalState(() => {
  const webParams = computed(() => new URLSearchParams(window.location.search))
  const paramsFull = computed<FullWebArgs>(() =>
    parseWebAppOptionsFromSearchParams(webParams.value),
  )

  const ydocUrl = computed(
    () => (paramsFull.value.engine.ydocUrl || resolveEnvUrl($config.YDOC_SERVER_URL)) ?? null,
  )

  return proxyRefs({
    params: paramsFull,
    ydocUrl,
  })
})
