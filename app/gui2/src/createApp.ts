import App from '@/App.vue'
import '@/assets/main.css'
import type { ApplicationConfig } from '@/util/config'
import { createPinia } from 'pinia'
import { createApp } from 'vue'
import Vue3Toastify, { type ToastContainerOptions } from 'vue3-toastify'

export function mountProjectApp(rootProps: {
  config: ApplicationConfig
  accessToken: string | null
  metadata?: object | undefined
  unrecognizedOptions: string[]
}) {
  const app = createApp(App, rootProps)
  app.use(createPinia())
  app.use(Vue3Toastify, {
    position: 'top-center',
    theme: 'light',
    closeOnClick: false,
    draggable: false,
    toastClassName: 'text-sm leading-170 bg-frame-selected rounded-2xl backdrop-blur-3xl',
  } as ToastContainerOptions)
  app.mount('#app')
  return app
}
