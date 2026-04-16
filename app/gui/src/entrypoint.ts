import './beforeMain' // Keep newline below to ensure that this import is always first.

import '#/styles.css'
import '#/tailwind.css'
import App from '$/App.vue'
import { setupLogger } from '$/log'
import { widgetDevtools } from '$/providers/openedProjects/widgetRegistry/devtools'
import router from '$/router'
import { createQueryClient } from '$/utils/queryClient'
import * as sentry from '@sentry/vue'
import type { Vue } from '@sentry/vue/types/types'
import { VueQueryPlugin } from '@tanstack/vue-query'
import { Path } from 'enso-common/src/services/Backend'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import * as detect from 'enso-common/src/utilities/detect'
import * as idbKeyval from 'idb-keyval'
import { createApp, markRaw } from 'vue'

/** The fraction of non-erroring interactions that should be sampled by Sentry. */
const SENTRY_SAMPLE_RATE = 0.005
const INITIAL_URL_KEY = `Enso-initial-url`

markRaw(HttpClient.prototype)

async function main() {
  setupLogger()
  const onAuthenticated = imNotSureButPerhapsFixingRefreshingWithAuthentication()
  const queryClient = await createQueryClientOfPersistCache()
  const rootDirPath = await getRootDirPath()
  const defaultDownloadPath = await getDefaultDownloadPath()

  const app = createApp(App)
  setupSentry(app)
  app.use(VueQueryPlugin, { queryClient, enableDevtoolsV6Plugin: true })
  app.use(router)
  app.use(widgetDevtools)
  app.provide('rootDirPath', rootDirPath)
  app.provide('defaultDownloadPath', defaultDownloadPath)
  app.provide('onAuthenticated', onAuthenticated)
  app.mount('#enso-app')
}

function setupSentry(app: Vue) {
  if (!detect.IS_DEV_MODE && $config.SENTRY_DSN) {
    sentry.init({
      dsn: $config.SENTRY_DSN,
      environment: $config.ENVIRONMENT ?? 'dev',
      release: $config.VERSION ?? 'dev',
      integrations: [
        sentry.browserTracingIntegration({ router }),
        sentry.extraErrorDataIntegration({ captureErrorCause: true }),
        sentry.replayIntegration(),
        new sentry.BrowserProfilingIntegration(),
      ],
      app,
      profilesSampleRate: SENTRY_SAMPLE_RATE,
      tracesSampleRate: SENTRY_SAMPLE_RATE,
      replaysSessionSampleRate: SENTRY_SAMPLE_RATE,
      replaysOnErrorSampleRate: 1.0,
    })
  }
}

function createQueryClientOfPersistCache() {
  const store = idbKeyval.createStore('enso', 'query-persist-cache')
  return createQueryClient({
    persisterStorage: {
      getItem: async (key) => idbKeyval.get(key, store),
      setItem: async (key, value) => idbKeyval.set(key, value, store),
      removeItem: async (key) => idbKeyval.del(key, store),
      clear: () => idbKeyval.clear(store),
      entries: () => idbKeyval.entries(store),
    },
  })
}

function imNotSureButPerhapsFixingRefreshingWithAuthentication() {
  /**
   * Note: Signing out always redirects to `/`. It is impossible to make this work,
   * as it is not possible to distinguish between having just logged out, and explicitly
   * opening a page with no URL parameters set.
   *
   * Client-side routing endpoints are explicitly not supported for live-reload, as they are
   * transitional pages that should not need live-reload when running `gui watch`.
   */
  const url = new URL(location.href)
  const isInAuthenticationFlow = url.searchParams.has('code') && url.searchParams.has('state')
  const authenticationUrl = location.href

  if (isInAuthenticationFlow) {
    history.replaceState(null, '', localStorage.getItem(INITIAL_URL_KEY))
  }
  if (isInAuthenticationFlow) {
    history.replaceState(null, '', authenticationUrl)
  } else {
    localStorage.setItem(INITIAL_URL_KEY, location.href)
  }

  function onAuthenticated() {
    if (isInAuthenticationFlow) {
      const initialUrl = localStorage.getItem(INITIAL_URL_KEY)
      if (initialUrl != null) {
        // This is not used past this point, however it is set to the initial URL
        // to make refreshing work as expected.
        history.replaceState(null, '', initialUrl)
      }
    }
  }
  return onAuthenticated
}

async function getRootDirPath() {
  const supportsLocalBackend =
    window.overrideFeatureFlags?.enableLocalBackend ?? $config.CLOUD_BUILD !== 'true'
  if (!supportsLocalBackend) return undefined
  const rootDirRequest = await fetch(`/api/root-directory-path`)
  return await rootDirRequest.text()
}

async function getDefaultDownloadPath() {
  const response = await fetch('/api/download-directory-path')
  return Path(await response.text())
}

main()
