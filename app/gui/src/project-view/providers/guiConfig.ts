import { proxyRefs } from '@/util/reactivity'
import { createGlobalState } from '@vueuse/core'
import {
  iterateOptions,
  OPTIONS,
  parseWebAppOptionsFromSearchParams,
  type ArgsFromOptions,
} from 'enso-common/src/options'
import { computed } from 'vue'

function resolveEnvUrl(url: string | undefined) {
  return url?.replace('__HOSTNAME__', window.location.hostname)
}

type FullWebArgs = ArgsFromOptions<typeof OPTIONS>
export type GuiConfig = ReturnType<typeof injectGuiConfig>
export const injectGuiConfig = createGlobalState(() => {
  const webParams = computed(() => new URLSearchParams(window.location.search))
  const parsed = computed(() => parseWebAppOptionsFromSearchParams(webParams.value, OPTIONS))
  // Fill defaults for convenience: materialize current values by merging defaults with parsed
  const paramsFull = computed<FullWebArgs>(() => {
    const out: any = {}
    const join = (p: string[]) => p.join('.')
    iterateOptions(OPTIONS, (path, node) => {
      if (path.length === 0) return
      const key = join(path)
      const raw = (parsed.value as any)[key]
      const val = raw != null ? raw : node.defaultValue
      let target: any = out
      for (let i = 0; i < path.length - 1; i++) {
        const seg = path[i]!
        target[seg] ??= {}
        target = target[seg]
      }
      const leaf = path[path.length - 1]!
      target[leaf] = val
    })
    return out as FullWebArgs
  })

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
