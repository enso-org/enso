/** @file Entry point into the cloud dashboard. */
import * as authentication from 'enso-authentication'

import * as detect from 'enso-common/src/detect'

import './tailwind.css'

// =================
// === Constants ===
// =================

/** Path to the SSE endpoint over which esbuild sends events. */
const ESBUILD_PATH = './esbuild'
/** SSE event indicating a build has finished. */
const ESBUILD_EVENT_NAME = 'change'
/** Path to the service worker that resolves all extensionless paths to `/index.html`.
 * This service worker is required for client-side routing to work when doing local development. */
const SERVICE_WORKER_PATH = './serviceWorker.js'

// ===================
// === Live reload ===
// ===================

if (detect.IS_DEV_MODE && (!(typeof IS_VITE !== 'undefined') || !IS_VITE)) {
    new EventSource(ESBUILD_PATH).addEventListener(ESBUILD_EVENT_NAME, () => {
        // This acts like `location.reload`, but it preserves the query-string.
        // The `toString()` is to bypass a lint without using a comment.
        location.href = location.href.toString()
    })
    void navigator.serviceWorker.register(SERVICE_WORKER_PATH)
} else {
    void navigator.serviceWorker
        .getRegistration()
        .then(serviceWorker => serviceWorker?.unregister())
}

// ===================
// === Entry point ===
// ===================

authentication.run({
    logger: console,
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
