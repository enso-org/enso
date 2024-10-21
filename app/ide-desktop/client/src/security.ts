/**
 * @file Security configuration of Electron. Most of the options are based on the official security
 * guide: https://www.electronjs.org/docs/latest/tutorial/security.
 */

import * as electron from 'electron'

// =================
// === Constants ===
// =================

/** The list of hosts that the app can access. They are required for user authentication to work. */
const TRUSTED_HOSTS = [
  'accounts.google.com',
  'accounts.youtube.com',
  'github.com',
  'production-enso-domain.auth.eu-west-1.amazoncognito.com',
  'production-enso-organizations-files.s3.amazonaws.com',
  'pb-enso-domain.auth.eu-west-1.amazoncognito.com',
  '7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com',
  'lkxuay3ha1.execute-api.eu-west-1.amazonaws.com',
  '8rf1a7iy49.execute-api.eu-west-1.amazonaws.com',
  'opk1cxpwec.execute-api.eu-west-1.amazonaws.com',
  'xw0g8j3tsb.execute-api.eu-west-1.amazonaws.com',
  's3.eu-west-1.amazonaws.com',
  // This (`localhost`) is required to access Project Manager HTTP endpoints.
  // This should be changed appropriately if the Project Manager's port number becomes dynamic.
  '127.0.0.1:30535',
]

/** The list of protocols that the app can open external links to. */
const TRUSTED_EXTERNAL_PROTOCOLS = ['mailto:', 'http:', 'https:']

/** The list of URLs a new WebView can be pointed to. */
const WEBVIEW_URL_WHITELIST: string[] = []

// =============
// === Utils ===
// =============

/**
 * Secure the web preferences of a new window. It deletes potentially unsecure options, making them
 * revert to secure defaults.
 */
export function secureWebPreferences(webPreferences: electron.WebPreferences) {
  delete webPreferences.preload
  delete webPreferences.nodeIntegration
  delete webPreferences.nodeIntegrationInWorker
  delete webPreferences.webSecurity
  delete webPreferences.allowRunningInsecureContent
  delete webPreferences.experimentalFeatures
  delete webPreferences.enableBlinkFeatures
  delete webPreferences.contextIsolation
  return webPreferences
}

// ================
// === Security ===
// ================

/**
 * Enabling sandbox globally. Follow the link to learn more:
 * https://www.electronjs.org/docs/latest/tutorial/sandbox.
 */
function enableGlobalSandbox() {
  electron.app.enableSandbox()
}

/**
 * By default, Electron will automatically approve all permission requests unless the developer has
 * manually configured a custom handler. While a solid default, security-conscious developers might
 * want to assume the very opposite. Follow the link to learn more:
 * https://www.electronjs.org/docs/latest/tutorial/security#5-handle-session-permission-requests-from-remote-content.
 */
function rejectPermissionRequests() {
  void electron.app.whenReady().then(() => {
    electron.session.defaultSession.setPermissionRequestHandler((_webContents, permission) => {
      console.error(`Unhandled permission request '${permission}'.`)
    })
  })
}

/**
 * This Electron app is configured with extra CORS headers. Those headers are added because they
 * increase security and enable higher resolution for `performance.now()` timers. However, one of
 * these headers (i.e., `Cross-Origin-Embedder-Policy: require-corp`), breaks the Stripe.js library.
 * This is because Stripe.js does not provide a `Cross-Origin-Resource-Policy` header or
 * `Cross-Origin-Embedder-Policy` header on resources hosted at `https://js.stripe.com`. Without
 * these headers, the browser will not load the resources. To fix this without compromising security
 * or profiling capabilities, add the missing headers to the Stripe.js resources by intercepting the
 * response headers.
 *
 * At the time of writing, these are the Stripe.js resources in question:
 *
 * - https://js.stripe.com/v3/m-outer-3437aaddcdf6922d623e172c2d6f9278.html
 * - https://js.stripe.com/v3/fingerprinted/js/embedded-checkout-outer-d5c7fe9d44281b88cdffdf803de759f1.js
 * - https://js.stripe.com/v3/controller-a8f00e403bc9538a7c1880ae6b6a2dc3.html
 *
 * The missing headers are added more generally to all resources hosted at `https://js.stripe.com`.
 * This is because the resources are fingerprinted and the fingerprint changes every time the
 * resources are updated. Additionally, Stripe.js may choose to add more resources in the future.
 */
function addMissingCorsHeaders() {
  void electron.app.whenReady().then(() => {
    electron.session.defaultSession.webRequest.onHeadersReceived((details, callback) => {
      details.responseHeaders = details.responseHeaders ?? {}
      if (details.url.includes('https://js.stripe.com/v3/')) {
        details.responseHeaders['Cross-Origin-Resource-Policy'] = ['cross-origin']
        details.responseHeaders['Cross-Origin-Embedder-Policy'] = ['require-corp']
      }
      callback({ responseHeaders: details.responseHeaders })
    })
  })
}

/**
 * A WebView created in a renderer process that does not have Node.js integration enabled will not
 * be able to enable integration itself. However, a WebView will always create an independent
 * renderer process with its own webPreferences. It is a good idea to control the creation of new
 * <webview> tags from the main process and to verify that their webPreferences do not disable
 * security features. Follow the link to learn more:
 * https://www.electronjs.org/docs/tutorial/security#11-verify-webview-options-before-creation.
 */
function limitWebViewCreation() {
  electron.app.on('web-contents-created', (_event, contents) => {
    contents.on('will-attach-webview', (event, webPreferences, params) => {
      secureWebPreferences(webPreferences)
      if (params.src != null && !WEBVIEW_URL_WHITELIST.includes(params.src)) {
        console.error(`Blocked the creation of WebView pointing to '${params.src}'`)
        event.preventDefault()
      }
    })
  })
}

/**
 * Navigation is a common attack vector. If an attacker can convince your app to navigate away from
 * its current page, they can possibly force your app to open web sites on the Internet. Follow the
 * link to learn more:
 * https://www.electronjs.org/docs/tutorial/security#12-disable-or-limit-navigation.
 */
function preventNavigation() {
  let lastFocusedWindow = electron.BrowserWindow.getFocusedWindow()
  electron.app.on('browser-window-focus', () => {
    lastFocusedWindow = electron.BrowserWindow.getFocusedWindow()
  })
  electron.app.on('web-contents-created', (_event, contents) => {
    contents.on('will-navigate', (event, navigationUrl) => {
      const parsedUrl = new URL(navigationUrl)
      const currentWindowUrl = lastFocusedWindow?.webContents.getURL()
      const parsedCurrentWindowUrl = currentWindowUrl != null ? new URL(currentWindowUrl) : null
      if (
        parsedUrl.origin !== parsedCurrentWindowUrl?.origin &&
        !TRUSTED_HOSTS.includes(parsedUrl.host)
      ) {
        event.preventDefault()
        console.error(`Prevented navigation to '${navigationUrl}'.`)
      }
    })
  })
}

/**
 * Much like navigation, the creation of new webContents is a common attack vector. Attackers
 * attempt to convince your app to create new windows, frames, or other renderer processes with
 * more privileges than they had before or with pages opened that they couldn't open before.
 * Follow the link to learn more:
 * https://www.electronjs.org/docs/tutorial/security#13-disable-or-limit-creation-of-new-windows.
 */
function disableNewWindowsCreation() {
  electron.app.on('web-contents-created', (_event, contents) => {
    contents.setWindowOpenHandler(details => {
      const { url } = details
      const parsedUrl = new URL(url)
      if (TRUSTED_EXTERNAL_PROTOCOLS.includes(parsedUrl.protocol)) {
        void electron.shell.openExternal(url)
        return { action: 'deny' }
      } else {
        console.error(`Blocking new window creation request to '${url}'.`)
        return { action: 'deny' }
      }
    })
  })
}

/** Enable all security settings. */
export function enableAll() {
  enableGlobalSandbox()
  rejectPermissionRequests()
  addMissingCorsHeaders()
  limitWebViewCreation()
  preventNavigation()
  disableNewWindowsCreation()
}
