/** @file Entry point into the cloud dashboard. */
import '#/tailwind.css'

import * as main from '#/index'

// ===================
// === Entry point ===
// ===================

main.run({
  logger: console,
  // Browsers usually do not support vibrancy for webpages.
  vibrancy: false,
  // This file is only included when building for the cloud.
  supportsLocalBackend: false,
  supportsDeepLinks: false,
  isAuthenticationDisabled: false,
  shouldShowDashboard: true,
  initialProjectName: null,
  /** The `onAuthenticated` option is mandatory but is not needed here,
   * so this function is empty. */
  onAuthenticated() {
    // eslint-disable-next-line @typescript-eslint/no-empty-function
  },
  /** The cloud frontend is not capable of running a Project Manager. */
  projectManagerUrl: null,
  ydocUrl: null,
  // This cannot be `appRunner: window.enso` as `window.enso` is set to a new value
  // every time a new project is opened.
  appRunner: {
    stopApp: () => {
      window.enso?.stopApp()
    },
    runApp: async (config, accessToken) => {
      await window.enso?.runApp(config, accessToken)
    },
  },
})
