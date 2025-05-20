import './beforeMain' // Keep newline below to ensure that this import is always first.

import '#/styles.css'
import '#/tailwind.css'
import App from '$/App.vue'
import router from '$/router.tsx'
import * as sentry from '@sentry/vue'
import { VueQueryPlugin } from '@tanstack/vue-query'
import * as detect from 'enso-common/src/detect'
import { createQueryClient } from 'enso-common/src/queryClient'
import { MotionGlobalConfig } from 'framer-motion'
import * as idbKeyval from 'idb-keyval'
import { createApp } from 'vue'

const HTTP_STATUS_BAD_REQUEST = 400
const API_HOST = $config.API_URL != null ? new URL($config.API_URL).host : null
/** The fraction of non-erroring interactions that should be sampled by Sentry. */
const SENTRY_SAMPLE_RATE = 0.005
const SCAM_WARNING_TIMEOUT = 1000
const INITIAL_URL_KEY = `Enso-initial-url`

async function main() {
  setupScamWarning()
  setupSentry()
  configureAnimations()
  const onAuthenticated = imNotSureButPerhapsFixingRefreshingWithAuthentication()
  const queryClient = createQueryClientOfPersistCache()
  const rootDirPath = await getRootDirPath()

  const app = createApp(App, { onAuthenticated, rootDirPath })
  app.use(VueQueryPlugin, { queryClient })
  app.use(router)
  app.mount('#enso-app')
}

function setupScamWarning() {
  function printScamWarning() {
    if (process.env.NODE_ENV === 'development') return
    const headerCss = `
      color: white;
      background: crimson;
      display: block;
      border-radius: 8px;
      font-weight: bold;
      padding: 10px 20px 10px 20px;
    `
      .trim()
      .replace(/\n\s+/, ' ')
    const headerCss1 = headerCss + ' font-size: 46px;'
    const headerCss2 = headerCss + ' font-size: 20px;'
    const msgCSS = 'font-size: 16px;'

    const msg1 =
      'This is a browser feature intended for developers. If someone told you to ' +
      'copy-paste something here, it is a scam and will give them access to your ' +
      'account and data.'
    const msg2 = 'See https://enso.org/selfxss for more information.'
    console.log('%cStop!', headerCss1)
    console.log('%cYou may be the victim of a scam!', headerCss2)
    console.log('%c' + msg1, msgCSS)
    console.log('%c' + msg2, msgCSS)
  }

  printScamWarning()
  let scamWarningHandle = 0

  window.addEventListener('resize', () => {
    window.clearTimeout(scamWarningHandle)
    scamWarningHandle = window.setTimeout(printScamWarning, SCAM_WARNING_TIMEOUT)
  })
}

function setupSentry() {
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

function configureAnimations() {
  const areAnimationsDisabled =
    window.DISABLE_ANIMATIONS === true ||
    localStorage.getItem('disableAnimations') === 'true' ||
    false

  MotionGlobalConfig.skipAnimations = areAnimationsDisabled

  if (areAnimationsDisabled) {
    document.documentElement.classList.add('disable-animations')
  } else {
    document.documentElement.classList.remove('disable-animations')
  }
}

function createQueryClientOfPersistCache() {
  const store = idbKeyval.createStore('enso', 'query-persist-cache')
  return createQueryClient({
    persisterStorage: {
      getItem: async (key) => idbKeyval.get(key, store),
      setItem: async (key, value) => idbKeyval.set(key, value, store),
      removeItem: async (key) => idbKeyval.del(key, store),
      clear: async () => idbKeyval.clear(store),
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
  const supportsLocalBackend = $config.CLOUD_BUILD !== 'true'
  if (!supportsLocalBackend) return undefined
  const rootDirRequest = await fetch(`/api/root-directory`)
  return await rootDirRequest.text()
}

main()
