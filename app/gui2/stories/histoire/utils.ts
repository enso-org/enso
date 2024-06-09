// @ts-nocheck
import { getCurrentInstance, ref, type App } from 'vue'

export const isDark = ref(false)

if (!window.__hst_controls_dark) {
  // eslint-disable-next-line camelcase
  window.__hst_controls_dark = []
}

// There could be multiple instances of the controls lib (in the controls book https://controls.histoire.dev)
window.__hst_controls_dark.push(isDark)

window.__hst_controls_dark_ready?.()

export function createSetupComponent(setup: (app: App) => void) {
  return () => {
    const app = getCurrentInstance()?.appContext.app
    if (app) setup(app)
    return null
  }
}
