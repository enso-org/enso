import App from '@/App.vue'
import '@/assets/main.css'
import type { ApplicationConfig, StringConfig } from '@/util/config'
import { createPinia } from 'pinia'
import { createApp } from 'vue'

export function mountProjectApp(rootProps: {
  config: StringConfig | null
  accessToken: string | null
  metadata?: object | undefined
  unrecognizedOptions: string[]
  appConfig: ApplicationConfig
}) {
  const app = createApp(App, rootProps)
  app.use(createPinia())
  app.mount('#app')
  return app
}
