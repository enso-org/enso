import { initializePrefixes } from '@/util/ast/node'
import { initializeFFI } from 'shared/ast/ffi'

import App from '@/App.vue'
import type { ApplicationConfig } from '@/util/config'
import { createPinia, type Pinia } from 'pinia'
import { createApp } from 'vue'

import '@/assets/main.css'

export async function mountProjectApp(
  rootProps: {
    config: ApplicationConfig
    accessToken: string | null
    unrecognizedOptions: string[]
  },
  pinia?: Pinia | undefined,
) {
  await initializeFFI()
  initializePrefixes()

  const usedPinia = pinia ?? createPinia()
  const app = createApp(App, rootProps)
  app.use(usedPinia)
  app.mount('#app')
  return () => {
    app.unmount()
    console.log('app unmounted')
    disposePinia(usedPinia)
  }
}

// Hack: `disposePinia` is not yet officially released, but we desperately need this for correct app
// cleanup. Pasted code from git version seems to work fine. This should be replaced with pinia
// export once it is available. Code copied from:
// https://github.com/vuejs/pinia/blob/8835e98173d9443531a7d65dfed09c2a8c19975d/packages/pinia/src/createPinia.ts#L74
export function disposePinia(pinia: Pinia) {
  const anyPinia: any = pinia
  anyPinia._e.stop()
  anyPinia._s.clear()
  anyPinia._p.splice(0)
  anyPinia.state.value = {}
  anyPinia._a = null
}
