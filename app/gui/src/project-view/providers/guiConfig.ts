import { proxyRefs } from '$/utils/reactivity'
import { createGlobalState } from '@vueuse/core'
import { parseWebAppOptionsFromSearchParams, type Options } from 'enso-common/src/options'
import { computed } from 'vue'

type FullWebArgs = Options
export type GuiConfig = ReturnType<typeof injectGuiConfig>
export const injectGuiConfig = createGlobalState(() => {
  const webParams = computed(() => new URLSearchParams(window.location.search))

  return proxyRefs({
    params: computed<FullWebArgs>(() => parseWebAppOptionsFromSearchParams(webParams.value)),
  })
})
