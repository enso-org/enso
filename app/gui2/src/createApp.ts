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

  const app = createApp(App, rootProps)
  app.use(pinia ?? createPinia())
  app.mount('#app')
  return app
}
