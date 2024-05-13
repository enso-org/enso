import { initializeFFI } from 'shared/ast/ffi'

import App from '@/App.vue'
import type { StringConfig } from '@/util/config'
import { createPinia, type Pinia } from 'pinia'
import { createApp, type ComponentPublicInstance, type App as VueApp } from 'vue'

import '@/assets/main.css'

export type RootProps = {
  config: StringConfig
  projectId: string
  logEvent: LogEvent
  hidden: boolean
  ignoreParamsRegex?: RegExp
}

export function AsyncApp() {
  return Promise.all([initializeFFI(), import('@/App.vue')]).then(([_, app]) => app)
}

export function setupApp(app: VueApp) {
  const pinia = createPinia()
  app.use(pinia)
}

export async function createProjectApp() {
  await initializeFFI()

  let activePinia: Pinia | null = null
  let app: VueApp | null = null
  let root: ComponentPublicInstance | null = null
  const unmount = () => {
    app?.unmount()
    if (activePinia) disposePinia(activePinia)
    activePinia = null
    app = null
  }
  return {
    unmount,
    mountOrUpdate: (props: RootProps, pinia?: Pinia | undefined) => {
      const piniaToUse = pinia ?? activePinia ?? createPinia()
      if (app == null || root == null || piniaToUse !== activePinia) {
        unmount()
        activePinia = piniaToUse
        console.log('createApp')
        app = createApp(App, props)
        app.use(activePinia)
        root = app.mount('#app')
      } else {
        console.log('update')
        Object.assign(root.$props, props)
      }
    },
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
