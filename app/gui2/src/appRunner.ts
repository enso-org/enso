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
  // function onUnrecognizedOption(path: string[]) {
  //   unrecognizedOptions.push(path.join('.'))
  // }
  // FIXME: https://github.com/enso-org/enso/issues/8610
  // Currently, options are provided that are not relevant to GUI2. These options cannot be removed
  // until GUI1 is removed, as GUI1 still needs them.
  const intermediateConfig = mergeConfig(baseConfig, urlParams())
  const appConfig = mergeConfig(intermediateConfig, config ?? {})
  const app = await mountProjectApp(
    {
      config: appConfig,
      accessToken,
      unrecognizedOptions,
    },
    pinia,
  )
  unmount = () => app.unmount()
}

function stopApp() {
  running = false
  unmount?.()
  unmount = null
}

export const appRunner = { runApp, stopApp }
