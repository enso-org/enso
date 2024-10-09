import * as dashboard from '#/index'
import '#/tailwind.css'
import { AsyncApp } from '@/asyncApp'
import { baseConfig, configValue, mergeConfig } from '@/util/config'
import { urlParams } from '@/util/urlParams'
import * as vueQuery from '@tanstack/vue-query'
import { isOnLinux } from 'enso-common/src/detect'
import * as commonQuery from 'enso-common/src/queryClient'
import { lazyVueInReact } from 'veaury'
import { type App } from 'vue'

const INITIAL_URL_KEY = `Enso-initial-url`
const SCAM_WARNING_TIMEOUT = 1000
export const isDevMode = process.env.NODE_ENV === 'development'

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

/** The entrypoint into the IDE. */
function main() {
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

  const resolveEnvUrl = (url: string | undefined) =>
    url?.replace('__HOSTNAME__', window.location.hostname)

  const config = configValue(mergeConfig(baseConfig, urlParams()))
  const supportsVibrancy = config.window.vibrancy
  const shouldUseAuthentication = config.authentication.enabled
  const projectManagerUrl =
    (config.engine.projectManagerUrl || resolveEnvUrl(PROJECT_MANAGER_URL)) ?? null
  const ydocUrl = (config.engine.ydocUrl || resolveEnvUrl(YDOC_SERVER_URL)) ?? null
  const initialProjectName = config.startup.project || null
  const urlWithoutStartupProject = new URL(location.toString())
  urlWithoutStartupProject.searchParams.delete('startup.project')
  history.replaceState(null, '', urlWithoutStartupProject)
  const queryClient = commonQuery.createQueryClient()

  const registerPlugins = (app: App) => {
    app.use(vueQuery.VueQueryPlugin, { queryClient })
  }

  const appRunner = lazyVueInReact(AsyncApp as any /* async VueComponent */, {
    beforeVueAppMount: (app) => registerPlugins(app as App),
  }) as dashboard.GraphEditorRunner

  dashboard.run({
    appRunner,
    logger: console,
    vibrancy: supportsVibrancy,
    supportsLocalBackend: !IS_CLOUD_BUILD,
    supportsDeepLinks: !isDevMode && !isOnLinux(),
    projectManagerUrl,
    ydocUrl,
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
    queryClient,
  })
}

main()
