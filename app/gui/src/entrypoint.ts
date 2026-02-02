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
import { onlineManager, QueryClient, queryOptions, VueQueryPlugin } from '@tanstack/vue-query'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import * as detect from 'enso-common/src/utilities/detect'
import * as idbKeyval from 'idb-keyval'
import { createApp, markRaw } from 'vue'

const HTTP_STATUS_BAD_REQUEST = 400
const API_HOST = $config.API_URL != null ? new URL($config.API_URL).host : null
/** The fraction of non-erroring interactions that should be sampled by Sentry. */
const SENTRY_SAMPLE_RATE = 0.005
const INITIAL_URL_KEY = `Enso-initial-url`

markRaw(HttpClient.prototype)

async function main() {
  setupLogger()
  const onAuthenticated = imNotSureButPerhapsFixingRefreshingWithAuthentication()
  const queryClient = await createQueryClientOfPersistCache()
  const remoteConfig = await getRemoteConfiguration(queryClient)
  const rootDirPath = await getRootDirPath()

  const app = createApp(App)
  setupSentry(app)
  app.use(VueQueryPlugin, { queryClient, enableDevtoolsV6Plugin: true })
  app.use(router)
  app.use(widgetDevtools)
  app.provide('rootDirPath', rootDirPath)
  app.provide('onAuthenticated', onAuthenticated)
  app.provide('remoteConfig', remoteConfig)
  app.mount('#enso-app')
}

function setupSentry(app: Vue) {
  if (!detect.IS_DEV_MODE && $config.SENTRY_DSN && $config.API_URL != null) {
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
      tracePropagationTargets: [$config.API_URL.split('//')[1] ?? ''],
      replaysSessionSampleRate: SENTRY_SAMPLE_RATE,
      replaysOnErrorSampleRate: 1.0,
      beforeSend: (event) => {
        if (
          (event.breadcrumbs ?? []).some(
            (breadcrumb) =>
              breadcrumb.type === 'http' &&
              breadcrumb.category === 'fetch' &&
              breadcrumb.data &&
              breadcrumb.data.status_code === HTTP_STATUS_BAD_REQUEST &&
              typeof breadcrumb.data.url === 'string' &&
              new URL(breadcrumb.data.url).host === API_HOST,
          )
        ) {
          return null
        }
        return event
      },
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

export interface RemoteConfig {
  ENSO_IDE_ENVIRONMENT?: string
  ENSO_IDE_API_URL?: string
  ENSO_IDE_AUTH_ENDPOINT?: string
  ENSO_IDE_STRIPE_KEY?: string
  ENSO_IDE_COGNITO_USER_POOL_ID?: string
  ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID?: string
  ENSO_IDE_COGNITO_DOMAIN?: string
  ENSO_IDE_COGNITO_REGION?: string
  ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID?: string
  ENSO_IDE_STRAVA_OAUTH_CLIENT_ID?: string
  ENSO_IDE_MS365_OAUTH_CLIENT_ID?: string
  ENSO_IDE_SALESFORCE_OAUTH_CLIENT_ID?: string
}

async function getRemoteConfiguration(queryClient: QueryClient) {
  const url = $config.API_URL ?? 'https://api.cloud.enso.org'

  const queryOpts = queryOptions<RemoteConfig>({
    queryKey: ['config', url],
    queryFn: ({ queryKey: [_, url] }) =>
      fetch(`${url}/utils/config`).then((response) => response.json()),
    retry: 5,
  })

  console.debug('About to fetch config')
  const config = await (onlineManager.isOnline() ?
    queryClient.fetchQuery(queryOpts)
  : queryClient.ensureQueryData(queryOpts))

  console.log('Environment:', config.ENSO_IDE_ENVIRONMENT)
  return config
}

main()
