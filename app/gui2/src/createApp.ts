import App from '@/App.vue'
import '@/assets/main.css'
import type { ApplicationConfig } from '@/util/config'
import { createPinia } from 'pinia'
import { createApp } from 'vue'

export function mountProjectApp(rootProps: {
  config: ApplicationConfig
  accessToken: string | null
  metadata?: object | undefined
  unrecognizedOptions: string[]
}) {
  const app = createApp(App, rootProps)
  app.use(createPinia())
  app.mount('#app')
  return app
}
