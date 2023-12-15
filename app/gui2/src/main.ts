import { baseConfig, mergeConfig } from '@/util/config'
import { isDevMode } from '@/util/detect'
import { urlParams } from '@/util/urlParams'
import { checkMinimumSupportedVersion } from '@/util/version'
import { run as runDashboard } from 'enso-authentication'
import { isOnLinux } from 'enso-common/src/detect'
import 'enso-dashboard/src/tailwind.css'

const INITIAL_URL_KEY = `Enso-initial-url`
const SCAM_WARNING_TIMEOUT = 1000

let unmount: null | (() => void) = null
let running = false

function printScamWarning() {
  if (isDevMode) return
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

export interface StringConfig {
  [key: string]: StringConfig | string
}

async function runApp(config: StringConfig | null, accessToken: string | null, metadata?: object) {
  running = true
  const { mountProjectApp } = await import('./createApp')
  if (!running) return
  unmount?.()
  const unrecognizedOptions: string[] = []
  function onUnrecognizedOption(path: string[]) {
    unrecognizedOptions.push(path.join('.'))
  }
  const applicationConfig = mergeConfig(baseConfig, urlParams(), { onUnrecognizedOption })
  const isVersionDeprecated = String(!(await checkMinimumSupportedVersion(applicationConfig)))
  if (!running) return
  const app = mountProjectApp({
    config: { ...config, isVersionDeprecated },
    accessToken,
    metadata,
    unrecognizedOptions,
    applicationConfig,
  })
  unmount = () => app.unmount()
}

function stopApp() {
  running = false
  unmount?.()
  unmount = null
}

const appRunner = { runApp, stopApp }

/** The entrypoint into the IDE. */
function main() {
  /** Note: Signing out always redirects to `/`. It is impossible to make this work,
   * as it is not possible to distinguish between having just logged out, and explicitly
   * opening a page with no URL parameters set.
   *
   * Client-side routing endpoints are explicitly not supported for live-reload, as they are
   * transitional pages that should not need live-reload when running `gui watch`. */
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

  const config = mergeConfig(baseConfig, urlParams())
  const shouldUseAuthentication = config.options.authentication.value
  const projectManagerUrl =
    config.groups.engine.options.projectManagerUrl.value || PROJECT_MANAGER_URL
  const initialProjectName = config.groups.startup.options.project.value || null

  runDashboard({
    appRunner,
    logger: console,
    // This entrypoint should never run in the cloud dashboard.
    supportsLocalBackend: true,
    supportsDeepLinks: !isDevMode && !isOnLinux(),
    projectManagerUrl,
    isAuthenticationDisabled: !shouldUseAuthentication,
    shouldShowDashboard: true,
    initialProjectName,
    onAuthenticated() {
      if (isInAuthenticationFlow) {
        const initialUrl = localStorage.getItem(INITIAL_URL_KEY)
        if (initialUrl != null) {
          // This is not used past this point, however it is set to the initial URL
          // to make refreshing work as expected.
          history.replaceState(null, '', initialUrl)
        }
      }
    },
  })
}

main()
