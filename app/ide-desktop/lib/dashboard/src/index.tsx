/** @file Entry point into the cloud dashboard. */
import * as authentication from 'enso-authentication'

import * as platform from 'enso-authentication/src/platform'

// =================
// === Constants ===
// =================

/** Path to the SSE endpoint over which esbuild sends events. */
const ESBUILD_PATH = '/esbuild'
/** SSE event indicating a build has finished. */
const ESBUILD_EVENT_NAME = 'change'
/** Path to the service worker that resolves all extensionless paths to `/index.html`.
 * This service worker is required for client-side routing to work when doing local development. */
const SERVICE_WORKER_PATH = '/serviceWorker.js'

// ===================
// === Live reload ===
// ===================

if (IS_DEV_MODE) {
    new EventSource(ESBUILD_PATH).addEventListener(ESBUILD_EVENT_NAME, () => {
        location.reload()
    })
    void navigator.serviceWorker.register(SERVICE_WORKER_PATH)
}

// ===================
// === Entry point ===
// ===================

authentication.run({
    logger: console,
    // This file is only included when building for the cloud,
    // so it is safe to set `platform` to `cloud`.
    platform: platform.Platform.cloud,
    showDashboard: true,
    // The `onAuthenticated` parameter is required but we don't need it, so we pass an empty function.
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    onAuthenticated() {},
})
