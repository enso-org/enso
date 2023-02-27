/** @file Security configuration of Electron. Most of the options are based on the official security
 * guide: https://www.electronjs.org/docs/latest/tutorial/security */

import Electron from 'electron'

// =================
// === Constants ===
// =================

/** The list of hosts that the app can access. They are required for user authentication to work. */
const trustedHosts = ['accounts.google.com', 'accounts.youtube.com', 'github.com']

/** The list of URLs a new WebView can be pointed to. */
const webViewUrlWhitelist: string[] = []

// =============
// === Utils ===
// =============

/** Secure the web preferences of a new window. It deletes potentially unsecure options, making them
 * revert to secure defaults. */
export function secureWebPreferences(webPreferences: Electron.WebPreferences) {
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

/** Enabling sandbox globally. Follow the link to learn more:
 * https://www.electronjs.org/docs/latest/tutorial/sandbox */
function enableGlobalSandbox() {
    Electron.app.enableSandbox()
}

/** By default, Electron will automatically approve all permission requests unless the developer has
 *  manually configured a custom handler. While a solid default, security-conscious developers might
 *  want to assume the very opposite. Follow the link to learn more:
 *  https://www.electronjs.org/docs/latest/tutorial/security#5-handle-session-permission-requests-from-remote-content */
function rejectPermissionRequests() {
    Electron.app.whenReady().then(() => {
        Electron.session.defaultSession.setPermissionRequestHandler(
            (webContents, permission, callback) => {
                console.error(`Unhandled permission request '${permission}'.`)
            }
        )
    })
}

/** A WebView created in a renderer process that does not have Node.js integration enabled will not
 *  be able to enable integration itself. However, a WebView will always create an independent
 *  renderer process with its own webPreferences. It is a good idea to control the creation of new
 *  <webview> tags from the main process and to verify that their webPreferences do not disable
 *  security features. Follow the link to learn more:
 *  https://www.electronjs.org/docs/tutorial/security#11-verify-webview-options-before-creation */
function limitWebViewCreation() {
    Electron.app.on('web-contents-created', (event, contents) => {
        contents.on('will-attach-webview', (event, webPreferences, params) => {
            secureWebPreferences(webPreferences)
            if (params.src && !webViewUrlWhitelist.includes(params.src)) {
                console.error(`Blocked the creation of WebView pointing to '${params.src}'`)
                event.preventDefault()
            }
        })
    })
}

/** Navigation is a common attack vector. If an attacker can convince your app to navigate away from
 * its current page, they can possibly force your app to open web sites on the Internet. Follow the
 * link to learn more:
 * https://www.electronjs.org/docs/tutorial/security#12-disable-or-limit-navigation */
function preventNavigation() {
    Electron.app.on('web-contents-created', (event, contents) => {
        contents.on('will-navigate', (event, navigationUrl) => {
            const parsedUrl = new URL(navigationUrl)
            if (parsedUrl.origin !== origin && !trustedHosts.includes(parsedUrl.host)) {
                event.preventDefault()
                console.error(`Prevented navigation to '${navigationUrl}'.`)
            }
        })
    })
}

/** Much like navigation, the creation of new webContents is a common attack vector. Attackers
 *  attempt to convince your app to create new windows, frames, or other renderer processes with
 *  more privileges than they had before or with pages opened that they couldn't open before.
 *  Follow the link to learn more:
 *  https://www.electronjs.org/docs/tutorial/security#13-disable-or-limit-creation-of-new-windows */
function disableNewWindowsCreation() {
    Electron.app.on('web-contents-created', (event, contents) => {
        contents.setWindowOpenHandler(({ url }) => {
            console.error(`Blocking new window creation request to '${url}'.`)
            return { action: 'deny' }
        })
    })
}

/** Enable all security settings. */
export function enableAll() {
    enableGlobalSandbox()
    rejectPermissionRequests()
    limitWebViewCreation()
    preventNavigation()
    disableNewWindowsCreation()
}
