import { baseConfig, mergeConfig, type StringConfig } from '@/util/config'
import { urlParams } from '@/util/urlParams'
import type { Pinia } from 'pinia'
import type { RootProps } from './createApp'

let stopQueued: ReturnType<typeof setTimeout> | null = null
let running = false
let queuedProps: RootProps | null = null

async function loadProjectApp() {
  const { createProjectApp } = await import('./createApp')
  return await createProjectApp()
}
const loadingPromise = loadProjectApp()
let projectApp: null | Awaited<typeof loadingPromise> = null

async function runApp(
  config: StringConfig | null,
  projectId: string,
  logEvent: LogEvent,
  ignoreKeysRegExp?: RegExp | undefined,
  pinia?: Pinia | undefined,
) {
  const unrecognizedOptions: string[] = []
  const intermediateConfig = mergeConfig(baseConfig, urlParams({ ignoreKeysRegExp }), {
    onUnrecognizedOption: (p) => unrecognizedOptions.push(p.join('.')),
  })
  const appConfig = mergeConfig(intermediateConfig, config ?? {})
  queuedProps = { config: appConfig, projectId, logEvent }

  if (stopQueued) {
    clearTimeout(stopQueued)
    stopQueued = null
  } else if (running) return

  if (!projectApp) {
    if (running) return
    running = true
    projectApp = await loadingPromise
  } else running = true

  if (running) {
    projectApp.mountOrUpdate(queuedProps, pinia)
  } else {
    projectApp.unmount()
    projectApp = null
  }
}

function stopApp() {
  if (!running) return
  stopQueued = setTimeout(() => {
    running = false
    projectApp?.unmount()
    projectApp = null
  }, 10)
}

export const appRunner = { runApp, stopApp }
