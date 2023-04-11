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

// ===================
// === Live relaod ===
// ===================

if (IS_DEV_MODE) {
    new EventSource(ESBUILD_PATH).addEventListener(ESBUILD_EVENT_NAME, () => {
        location.reload()
    })
    void navigator.serviceWorker.register('/serviceWorker.js')
}

// ===================
// === Entry point ===
// ===================

authentication.run({
    logger: console,
    // This file is only included when building for the cloud,
    // so it is safe to set `platform` to `cloud`.
    platform: platform.Platform.cloud,
    // The `onAuthenticated` parameter is required but we don't need it, so we pass an empty function.
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    onAuthenticated() {},
})
