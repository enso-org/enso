import { proxyRefs } from '@/util/reactivity'
import { createGlobalState } from '@vueuse/core'
import {
  mergeDefaultsWithFlat,
  parseWebAppOptionsFromSearchParams,
  type Options,
} from 'enso-common/src/options'
import { computed } from 'vue'

function resolveEnvUrl(url: string | undefined) {
  return url?.replace('__HOSTNAME__', window.location.hostname)
}

type FullWebArgs = Options
export type GuiConfig = ReturnType<typeof injectGuiConfig>
export const injectGuiConfig = createGlobalState(() => {
  const webParams = computed(() => new URLSearchParams(window.location.search))
  const parsed = computed(() => parseWebAppOptionsFromSearchParams(webParams.value))
  // Merge defaults with URL-provided flat overrides
  const paramsFull = computed<FullWebArgs>(() => mergeDefaultsWithFlat(parsed.value))

  const ydocUrl = computed(
    () => (paramsFull.value.engine.ydocUrl || resolveEnvUrl($config.YDOC_SERVER_URL)) ?? null,
  )

  const projectManagerUrl = computed(
    () =>
      (paramsFull.value.engine.projectManagerUrl || resolveEnvUrl($config.PROJECT_MANAGER_URL)) ??
      null,
  )
  return proxyRefs({
    params: paramsFull,
    ydocUrl,
    projectManagerUrl,
  })
})
