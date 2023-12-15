import AppRoot from '@/App.vue'
import '@/assets/main.css'
import type { HelpInfo } from '@/components/HelpScreen/types'
import type { StringConfig } from '@/main'
import { createPinia } from 'pinia'
import { createApp } from 'vue'

export function mountProjectApp(rootProps: {
  config: StringConfig | null
  accessToken: string | null
  metadata?: object | undefined
  helpInfo: HelpInfo | undefined
}) {
  const app = createApp(AppRoot, rootProps)
  app.use(createPinia())
  app.mount('#app')
  return app
}
