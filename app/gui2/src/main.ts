import CONFIG from '@/config.json' assert { type: 'json' }
import { isDevMode } from '@/util/detect'
import { run as runDashboard } from 'enso-authentication'
import { isOnLinux } from 'enso-common/src/detect'
import 'enso-dashboard/src/tailwind.css'
import { decodeQueryParams } from 'lib0/url'
import { options as cliOptions, objectToGroup } from 'runner/config'
import { urlParams } from 'runner/host'
import { version } from 'runner/version'

const vueAppEntry = import('./createApp')

const INITIAL_URL_KEY = `Enso-initial-url`

const params = decodeQueryParams(location.href)
const initialProjectName = params.project ?? null

let unmount: null | (() => void) = null
let runRequested = false

export interface StringConfig {
  [key: string]: StringConfig | string
}

async function runApp(config: StringConfig | null, accessToken: string | null, metadata?: object) {
  runRequested = true
  const { mountProjectApp } = await vueAppEntry
  if (runRequested) {
    unmount?.()
    const app = mountProjectApp({ config, accessToken, metadata })
    unmount = () => app.unmount()
  }
}

function stopApp() {
  runRequested = false
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

  const options = cliOptions.merge(objectToGroup(CONFIG, { Version: version }))
  const parseOk = options.loadAllAndDisplayHelpIfUnsuccessful([urlParams()])
  if (parseOk) {
    const shouldUseAuthentication = options.options.authentication.value
    const projectManagerUrl = options.groups.engine.options.projectManagerUrl.value || null

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
}

main()
