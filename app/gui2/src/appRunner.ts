import { baseConfig, mergeConfig, type StringConfig } from '@/util/config'
import { urlParams } from '@/util/urlParams'
import type { Pinia } from 'pinia'

let unmount: null | (() => void) = null
let running = false

async function runApp(
  config: StringConfig | null,
  accessToken: string | null,
  _metadata?: object | undefined,
  pinia?: Pinia | undefined,
) {
  running = true
  const { mountProjectApp } = await import('./createApp')
  if (!running) return
  unmount?.()
  const unrecognizedOptions: string[] = []
  function onUnrecognizedOption(path: string[]) {
    unrecognizedOptions.push(path.join('.'))
  }
  const intermediateConfig = mergeConfig(baseConfig, urlParams(), { onUnrecognizedOption })
  const appConfig = mergeConfig(intermediateConfig, config ?? {})
  unmount = await mountProjectApp({ config: appConfig, accessToken, unrecognizedOptions }, pinia)
}

function stopApp() {
  running = false
  unmount?.()
  unmount = null
}

export const appRunner = { runApp, stopApp }
